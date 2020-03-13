use crate::parser::lex::Lexer;
use crate::parser::{
    Argument, DataName, DataVariable, DataVariant, Grammar, Pattern, RuleBody, RuleName, RuleRef,
    Token,
};
use im::{vector, Vector};
use internship::IStr;
use rand::seq::IteratorRandom;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    convert::TryFrom,
    fs,
    io::{self, Read},
    path::Path,
};

// type State = Env<IStr, Vector<OutToken>>;
type State = HashMap<DataVariable, DataVariant>;

pub struct Generator {
    grammar: Grammar,
    rng: RefCell<rand::rngs::ThreadRng>,
    seen_sentences: HashSet<im::Vector<OutToken>>,
    pub(crate) max_trials: usize,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
enum OutToken {
    Sym(IStr),
    Plus, // For concatenation without space insertion.
}

impl From<OutToken> for Token {
    fn from(out_token: OutToken) -> Self {
        match out_token {
            OutToken::Sym(sym) => Token::StrLit(sym),
            OutToken::Plus => Token::Plus,
        }
    }
}

impl From<&OutToken> for Token {
    fn from(out_token: &OutToken) -> Self {
        out_token.clone().into()
    }
}

impl From<IStr> for OutToken {
    fn from(sym: IStr) -> Self {
        Self::Sym(sym)
    }
}

impl From<&IStr> for OutToken {
    fn from(sym: &IStr) -> Self {
        Self::Sym(sym.clone())
    }
}

const DEFAULT_MAX_TRIALS: usize = 1000;

enum ChoiceErr {
    UnboundRuleName,
    InexhaustivePattern,
}

impl Generator {
    pub fn new(grammar: Grammar) -> Self {
        Self {
            grammar,
            rng: RefCell::new(rand::thread_rng()),
            seen_sentences: HashSet::new(),
            max_trials: DEFAULT_MAX_TRIALS,
        }
    }

    fn choose_rule(
        &self,
        rule_name: &RuleName,
        arguments: &Vec<DataVariant>,
    ) -> Result<RuleBody, ChoiceErr> {
        // Collect all rules that *could* expand `token`.
        let rule = self
            .grammar
            .rule_decls
            .iter()
            .filter(|rule_decl| &rule_decl.signature.name == rule_name)
            .next() // TODO: Should this *only* return the first-found rule decl?
            .ok_or(ChoiceErr::UnboundRuleName)?;

        // Check each case (body) one after another until an allowable case is found.
        for case in &rule.bodies {
            let param_typs = &rule.signature.parameter_types;
            let reqs = &case.guard.requirements;
            if self.allowable(&param_typs, reqs, arguments) {
                return Ok(case.clone());
            }
        }
        // We are out of possibilities. This case must not have been handled in the case analysis.
        Err(ChoiceErr::InexhaustivePattern)
    }

    fn allowable(
        &self,
        _types: &Vec<DataName>,
        requirements: &Vec<Pattern>,
        arguments: &Vec<DataVariant>,
    ) -> bool {
        // TODO: add type checking here? When ready, use the currently-unused `_types` parameter.
        if requirements.len() != arguments.len() {
            // TODO: how should we handle missing types?
            //       Is `foo.X1.Y1` == `foo.X1` == `foo`?
            // TODO: add better context to this error i.e. what rule name? where was it called?
            panic!(
                "Wrong number of arguments! Got values {:?} but needed {} values!",
                arguments,
                requirements.len()
            );
        }

        requirements
            .iter()
            .zip(arguments.iter())
            .all(|(req, arg)| match req {
                // Pattern::Star (`.*`) matches against any actual variant.
                Pattern::Star => true,
                Pattern::Variant(v) => v == arg,
            })
    }

    /// Given a variable name, if the current state already has a binding for
    /// that variable, returns the `DataVariant` bound to the variable. If no
    /// binding yet exists, this fn randomly selects a `DataVariant` from the
    /// `DataDecl`'s listed variants, creates a new binding in the state, and
    /// returns the randomly selected `DataVariant`.
    fn value_of_variable<'st>(
        &self,
        var: &DataVariable,
        state: &'st mut State,
    ) -> &'st mut DataVariant {
        state.entry(var.clone()).or_insert_with(|| {
            let mut iter = self
                .grammar
                .data_decls
                .iter()
                .filter(|decl| decl.name.matches_variable(var));
            let res = iter
                .next()
                .expect(&format!("a rule matching {:?} exists", var));
            let more = iter.next();
            assert_eq!(
                more,
                None,
                "Ambiguous variable name {:?}! Could refer to either {:?} or {:?}",
                var,
                res.name,
                more.unwrap().name
            );
            res.variants
                .iter()
                .choose(&mut *self.rng.borrow_mut())
                .expect("no data decl has 0 variants")
                .clone()
        })
    }

    fn generate_non_unique_from_start(&self, start: RuleName) -> Vector<OutToken> {
        let start_call = RuleRef {
            rule: start,
            vars: vec![],
        };
        let start_sentence = vector![Token::RuleRef(start_call)];
        self.generate_non_unique_from_sentence(start_sentence)
    }

    fn generate_non_unique_from_sentence(&self, sentence: Vector<Token>) -> Vector<OutToken> {
        let mut state = HashMap::new();
        let mut new_sentence: Vector<OutToken> = Default::default();

        // For each token, if it's a variable, it needs to be replaced.
        for token in sentence {
            match token {
                Token::StrLit(sym) => new_sentence.push_back(sym.into()),
                Token::Plus => new_sentence.push_back(OutToken::Plus),
                Token::RuleRef(RuleRef { ref rule, ref vars }) => {
                    // Ensure each of `vars` has a binding.
                    let arguments: Vec<DataVariant> = vars
                        .iter()
                        .map(|arg| match arg {
                            // This is a case like `they.Number` where a variable is being passed in.
                            Argument::Variable(var) => {
                                self.value_of_variable(var, &mut state).clone()
                            }
                            // This is a case like `they.singular` where a data-variant is being passed in.
                            Argument::Variant(val) => val.clone(),
                        })
                        .collect();

                    // Search the grammar's rules via `self.choose_rule`.
                    // If no rule bodies are viable, panic.
                    let tokens_to_add: Vector<OutToken> = {
                        let body = match self.choose_rule(rule, &arguments) {
                            Ok(body) => body,
                            Err(err) => self.report_choice_error(err, rule, &arguments),
                        };
                        let next_sentence = body.sentential_form.clone();
                        self.generate_non_unique_from_sentence(next_sentence)
                    };

                    new_sentence.append(tokens_to_add);
                }
            }
        }

        new_sentence
    }

    /// TODO: exatract this logic into top-level error reporting, i.e. we shouldn't be `panic`ing here.
    fn report_choice_error(
        &self,
        err: ChoiceErr,
        rule: &RuleName,
        arguments: &Vec<DataVariant>,
    ) -> ! {
        match err {
            ChoiceErr::UnboundRuleName => {
                panic!("The identifier `{}` is not the name of a rule!", rule.0)
            }
            ChoiceErr::InexhaustivePattern => {
                let typ_names = self
                    .grammar
                    .rule_decls
                    .iter()
                    .filter(|decl| &decl.signature.name == rule)
                    .next()
                    .expect("rule to exist")
                    .signature
                    .parameter_types
                    .iter()
                    .map(|typ_name| typ_name.0.as_str());
                let arg_names = arguments.iter().map(|evald_arg| evald_arg.0.as_str());
                let bindings = arg_names
                    .zip(typ_names)
                    .map(|(value, typ)| format!("{} = .{}", typ, value))
                    .collect::<Vec<_>>();
                panic!(
                    "No case of rule `{}` matches the current arguments: {:?}",
                    rule.0, bindings
                )
            }
        }
    }

    /// Takes in a iterator of either `Sym`s or `Plus`es. Before every `Sym`,
    /// insert a space character. If there is a `Plus` before a `Sym`, don't
    /// insert the space. If it's at the beginning of the iterator, don't insert
    /// a space.
    fn join_symbols<'it>(&self, syms: impl Iterator<Item = &'it OutToken> + 'it) -> String {
        let (lower_bound, _upper) = syms.size_hint();
        let mut text = String::with_capacity(lower_bound);
        let mut add_joining_space = false;

        for sym in syms {
            match sym {
                OutToken::Sym(sym) => {
                    if add_joining_space {
                        text.push(' ');
                    }
                    text.push_str(sym.as_str());
                    add_joining_space = true;
                }
                OutToken::Plus => add_joining_space = false,
            }
        }
        text
    }

    pub fn generate(&mut self) -> Option<String> {
        let start = RuleName(IStr::new("start"));
        let mut trials = 0;
        loop {
            let sentence = self.generate_non_unique_from_start(start.clone());
            if !self.seen_sentences.contains(&sentence) {
                self.seen_sentences.insert(sentence.clone());
                let text = self.join_symbols(sentence.iter());
                break Some(text);
            } else if trials >= self.max_trials {
                break None;
            } else {
                trials += 1;
            }
        }
    }
}

impl TryFrom<&Path> for Generator {
    type Error = io::Error;

    fn try_from(path: &Path) -> Result<Self, Self::Error> {
        let mut src = String::new();
        let mut file = fs::File::open(path)?;
        file.read_to_string(&mut src)?;

        let mut lexer = Lexer::from(src.as_ref());
        let mut lexeme_buf = vec![];
        match lexer.to_slice(&mut lexeme_buf) {
            Ok(_) => (),
            Err(e) => panic!("Error encountered while tokenizing: {}", e),
        }
        let parse_res = crate::parser::parse_from_lex_stream(&lexeme_buf);

        let grammar = match parse_res {
            Ok((_rest, grammar)) => grammar,
            Err(e) => {
                eprintln!("Error encountered while parsing: {}", e);
                std::process::exit(-1);
            }
        };

        Ok(Self::new(grammar))
    }
}
