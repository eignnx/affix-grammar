use crate::fault::{DynamicErr, DynamicRes};
use crate::parser::syntax::{
    Argument, Case, DataName, DataVariable, DataVariant, Grammar, Guard, Pattern, RuleName,
    RuleRef, Token,
};
use im::{vector, Vector};
use internship::IStr;
use rand::seq::IteratorRandom;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

type State = HashMap<DataVariable, DataVariant>;

pub struct Generator {
    grammar: Grammar,
    rng: RefCell<rand::rngs::ThreadRng>,
    seen_sentences: HashSet<im::Vector<OutToken>>,
    pub max_trials: usize,
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

impl Generator {
    pub fn new(grammar: Grammar) -> Self {
        Self {
            grammar,
            rng: RefCell::new(rand::thread_rng()),
            seen_sentences: HashSet::new(),
            max_trials: DEFAULT_MAX_TRIALS,
        }
    }

    fn choose_rule<'gen, 'buf>(
        &'gen self,
        rule_name: &RuleName,
        arguments: &Vec<DataVariant>,
    ) -> DynamicRes<Case>
    where
        'buf: 'gen,
    {
        // Collect all rules that *could* expand `token`.
        let rule = self
            .grammar
            .rule_decls
            .iter()
            .filter(|rule_decl| &rule_decl.signature.name == rule_name)
            .next() // TODO: Should this *only* return the first-found rule decl?
            .ok_or(DynamicErr::unbound_rule_name(rule_name.0.as_str()))?;

        // Check each case one after another until an allowable case is found.
        for case in &rule.cases {
            let param_typs = &rule.signature.parameter_types;
            let reqs = &case.guard;
            if self.allowable(rule_name, &param_typs, reqs, arguments)? {
                return Ok(case.clone());
            }
        }
        // We are out of possibilities. This case must not have been handled in the case analysis.
        Err(DynamicErr::inexhaustive_case_analysis(
            &self.grammar.rule_decls,
            rule_name,
            arguments,
        ))
    }

    fn allowable(
        &self,
        rule_name: &RuleName,
        _types: &Vec<DataName>,
        guard: &Guard,
        arguments: &Vec<DataVariant>,
    ) -> DynamicRes<bool> {
        // TODO: add type checking here? When ready, use the currently-unused `_types` parameter.
        if guard.requirements.len() != arguments.len() {
            let arguments = arguments.iter().map(|val| val.0.as_str().into()).collect();
            return Err(DynamicErr::WrongArityRuleReference {
                rule_name: rule_name.0.as_str().into(),
                arguments,
                expected_len: guard.requirements.len(),
            });
        }

        let res = guard
            .requirements
            .iter()
            .zip(arguments.iter())
            .all(|(req, arg)| match req {
                // Pattern::Star (`.*`) matches against any actual variant.
                Pattern::Star => true,
                Pattern::Variant(v) => v == arg,
            });
        Ok(res)
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
    ) -> DynamicRes<&'st mut DataVariant> {
        Ok(state.entry(var.clone()).or_insert({
            let mut iter = self
                .grammar
                .data_decls
                .iter()
                .filter(|decl| decl.name.matches_variable(var));
            let res = iter
                .next()
                .expect(&format!("a rule matching {:?} exists", var));
            let more = iter.next();
            if let Some(more_name) = more {
                return Err(DynamicErr::AmbiguousSymbol {
                    symbol: var.0.to_string(),
                    possibility1: res.name.0.to_string(),
                    possibility2: more_name.name.0.to_string(),
                });
            }
            res.variants
                .keys()
                .choose(&mut *self.rng.borrow_mut())
                .expect("no data decl has 0 variants")
                .clone()
        }))
    }

    fn stringify_data_variant<'gen, 'buf>(
        &'gen self,
        variant: DataVariant,
    ) -> DynamicRes<Vector<OutToken>>
    where
        'buf: 'gen,
    {
        let decl = self
            .grammar
            .data_decls
            .iter()
            .filter(|decl| decl.variants.contains_key(&variant))
            .next()
            .ok_or(DynamicErr::UnboundSymbol {
                symbol: variant.0.as_str().into(),
            })?;
        let alternatives = decl
            .variants
            .get(&variant)
            .unwrap() // Can't panic because of previous `filter`.
            .clone();
        let backup = alternatives
            .iter()
            .choose(&mut *self.rng.borrow_mut())
            .ok_or(DynamicErr::NoDataVariantStringification {
                symbol: variant.0.as_str().into(),
            })?;
        self.generate_non_unique_from_sentence(backup.clone())
    }

    fn generate_non_unique_from_start<'gen, 'buf>(
        &'gen self,
        start: RuleName,
    ) -> DynamicRes<Vector<OutToken>>
    where
        'buf: 'gen,
    {
        let start_call = RuleRef {
            rule: start,
            vars: vec![],
        };
        let start_sentence = vector![Token::RuleRef(start_call)];
        self.generate_non_unique_from_sentence(start_sentence)
    }

    fn generate_non_unique_from_sentence<'gen, 'buf>(
        &'gen self,
        sentence: Vector<Token>,
    ) -> DynamicRes<Vector<OutToken>> {
        let mut state = HashMap::new();
        let mut new_sentence: Vector<OutToken> = Default::default();

        // For each token, if it's a variable, it needs to be replaced.
        for token in sentence {
            match token {
                Token::StrLit(sym) => new_sentence.push_back(sym.into()),
                Token::Plus => new_sentence.push_back(OutToken::Plus),
                Token::DataVariant(variant) => {
                    let to_append = self.stringify_data_variant(variant)?;
                    new_sentence.append(to_append);
                }
                Token::DataVariable(ref variable) => {
                    let variant = self.value_of_variable(variable, &mut state)?.clone();
                    let to_append = self.stringify_data_variant(variant)?;
                    new_sentence.append(to_append);
                }
                Token::RuleRef(RuleRef { ref rule, ref vars }) => {
                    // Ensure each of `vars` has a binding.
                    let mut arguments = vec![];
                    for arg in vars {
                        match arg {
                            // This is a case like `they.Number` where a variable is being passed in.
                            Argument::Variable(var) => {
                                let thing = self.value_of_variable(var, &mut state)?.clone();
                                arguments.push(thing);
                            }
                            // This is a case like `they.singular` where a data-variant is being passed in.
                            Argument::Variant(val) => arguments.push(val.clone()),
                        }
                    }

                    // Search the grammar's rules via `self.choose_rule`.
                    // If no rule cases are viable, panic.
                    let case = self.choose_rule(rule, &arguments)?;
                    let next_sentence =
                            case.alternatives.iter().choose(&mut *self.rng.borrow_mut())
                            .expect("Invariant violated by parser: should not be able to have an empty set of sentential form alternatives!");
                    let tokens_to_add =
                        self.generate_non_unique_from_sentence(next_sentence.clone())?;
                    new_sentence.append(tokens_to_add);
                }
            }
        }

        Ok(new_sentence)
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

    pub fn generate<'gen, 'buf>(&'gen mut self) -> DynamicRes<String>
    where
        'buf: 'gen,
    {
        let start = RuleName(IStr::new("start"));
        let mut trials = 0;
        loop {
            let sentence = self.generate_non_unique_from_start(start.clone())?;
            if !self.seen_sentences.contains(&sentence) {
                self.seen_sentences.insert(sentence.clone());
                let text = self.join_symbols(sentence.iter());
                break Ok(text);
            } else if trials >= self.max_trials {
                break Err(DynamicErr::MaxTrialsExceeded {
                    trials: self.max_trials,
                });
            } else {
                trials += 1;
            }
        }
    }
}
