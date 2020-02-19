use crate::env::Env;
use crate::parser::lex::Lexer;
use crate::parser::{DataVariant, Grammar, RuleBody, RuleName, RuleRef, Token};
use im::{vector, Vector};
use internship::IStr;
use rand::Rng;
use std::{
    cell::RefCell,
    collections::HashSet,
    convert::TryFrom,
    fs,
    io::{self, Read},
    path::Path,
};

type State = Env<IStr, Vector<OutToken>>;

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

impl Generator {
    pub fn new(grammar: Grammar) -> Self {
        Self {
            grammar,
            rng: RefCell::new(rand::thread_rng()),
            seen_sentences: HashSet::new(),
            max_trials: DEFAULT_MAX_TRIALS,
        }
    }

    fn symbol_eq_sentence(&self, sym: IStr, sentence: &Vector<OutToken>) -> bool {
        let joined = self.join_symbols(sentence.iter());
        joined == sym.as_str()
    }

    fn choose_rule(&self, rule_name: &RuleName, state: &State) -> Option<&RuleBody> {
        // Collect all rules that *could* expand `token`.
        let mut possibilities: Vec<&RuleBody> = self
            .grammar
            .rule_decls
            .iter()
            .filter(|rule_decl| &rule_decl.signature.name == rule_name)
            .map(|rule_decl| &rule_decl.bodies)
            .flatten()
            .collect();

        // Keep picking random rules until one is found which satisfies it's guard conditions.
        loop {
            if possibilities.is_empty() {
                return None;
            }
            let idx = self.rng.borrow_mut().gen_range(0, possibilities.len());
            let rule = possibilities[idx];
            if rule
                .guard
                .requirements
                .iter()
                .all(|req| self.allowable(req, state))
            {
                return Some(rule);
            } else {
                possibilities.swap_remove(idx);
            }
        }
    }

    fn allowable(&self, value: &DataVariant, state: &State) -> bool {
        // TODO: impl this
        true
    }

    fn generate_non_unique_from_start(
        &self,
        start: RuleName,
        state: &mut State,
    ) -> Vector<OutToken> {
        let start_call = RuleRef {
            rule: start,
            vars: vec![],
        };
        let start_sentence = vector![Token::RuleRef(start_call)];
        self.generate_non_unique_from_sentence(start_sentence, state)
    }

    /// Psuedo-code:
    /// Start with the start symbol. Call this `sentence`.
    /// Repeat until there are no non-terminals in `sentence`:
    ///     For each token in the sentence:
    ///         If it's a literal, keep it.
    ///         If it's a non-terminal:
    ///             Collect each rule that the non-terminal matches against.
    ///             Select one of those rules at random.
    ///             Append it's body onto the new sentence.
    fn generate_non_unique_from_sentence(
        &self,
        sentence: Vector<Token>,
        state: &mut State,
    ) -> Vector<OutToken> {
        let mut new_sentence: Vector<OutToken> = Default::default();

        // For each token, if it's a variable, it needs to be replaced.
        for token in sentence {
            match token {
                Token::StrLit(sym) => new_sentence.push_back(sym.into()),
                Token::Plus => new_sentence.push_back(OutToken::Plus),
                Token::RuleRef(RuleRef { ref rule, ref vars }) => {
                    // more_to_do = true; // We'll have to revisit.

                    // Search the grammar's rules via `self.choose_rule`.
                    // If no rule bodies are viable, panic.
                    state.push_frame_and_watch(vec![]);
                    // TODO: impl this!
                    // state.insert_local(key: K, value: V);
                    let tokens_to_add: Vector<OutToken> = {
                        let rule = self.choose_rule(rule, &state).unwrap_or_else(|| {
                            panic!("The identifier `{}` is not the name of a rule!", rule.0);
                        });
                        let next_sentence = rule.sentential_form.clone();
                        self.generate_non_unique_from_sentence(next_sentence, state)
                    };
                    state.pop_frame();

                    new_sentence.append(tokens_to_add);
                }
            }
        }

        new_sentence
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
            let sentence = self.generate_non_unique_from_start(start.clone(), &mut Env::new());
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
        lexer.to_slice(&mut lexeme_buf).expect("BAD TOKENIZEATION");
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
