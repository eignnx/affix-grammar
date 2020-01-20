use crate::syntax::{EvalStmt, Grammar, Rule, TestStmt, Token};
use im::{vector, Vector};
use rand::Rng;
use std::{
    cell::RefCell,
    collections::HashSet,
    convert::TryFrom,
    fs,
    io::{self, Read},
    path::Path,
    rc::Rc,
};
use string_interner::{StringInterner, Sym};

type State = im::HashMap<Sym, Vector<OutputSym>>;

pub type Syms = Rc<RefCell<StringInterner<Sym>>>;

pub fn make_symbol_pool() -> Syms {
    Rc::new(RefCell::new(StringInterner::default()))
}

pub struct Generator {
    grammar: Grammar,
    rng: RefCell<rand::rngs::ThreadRng>,
    symbol_pool: Syms,
    seen_sentences: HashSet<im::Vector<OutputSym>>,
    pub(crate) max_trials: usize,
}

#[derive(Clone, Hash, PartialEq, Eq)]
enum OutputSym {
    Sym(Sym),
    Plus, // For concatenation without space insertion.
}

impl From<OutputSym> for Token {
    fn from(output_sym: OutputSym) -> Self {
        match output_sym {
            OutputSym::Sym(sym) => Token::Lit(sym),
            OutputSym::Plus => Token::Plus,
        }
    }
}

impl From<&OutputSym> for Token {
    fn from(output_sym: &OutputSym) -> Self {
        output_sym.clone().into()
    }
}

impl From<Sym> for OutputSym {
    fn from(sym: Sym) -> Self {
        Self::Sym(sym)
    }
}

impl From<&Sym> for OutputSym {
    fn from(sym: &Sym) -> Self {
        Self::Sym(*sym)
    }
}

const DEFAULT_MAX_TRIALS: usize = 1000;

impl Generator {
    pub fn new(grammar: Grammar, symbol_pool: Syms) -> Self {
        Self {
            grammar,
            rng: RefCell::new(rand::thread_rng()),
            symbol_pool,
            seen_sentences: HashSet::new(),
            max_trials: DEFAULT_MAX_TRIALS,
        }
    }

    fn symbol_eq_sentence(&self, sym: Sym, sentence: &Vector<OutputSym>) -> bool {
        let joined = self.join_symbols(sentence.iter());
        self.intern(&joined) == sym
    }

    fn resolve(&self, sym: Sym) -> String {
        self.symbol_pool.borrow().resolve(sym).unwrap().into()
    }

    fn test(&self, stmt: &TestStmt, state: &State) -> bool {
        match stmt {
            TestStmt::Key(key, value) => {
                if let Some(stored) = state.get(key) {
                    self.symbol_eq_sentence(*value, stored)
                } else {
                    false
                }
            }
            TestStmt::NotKey(key, value) => {
                if let Some(stored) = state.get(&key) {
                    !self.symbol_eq_sentence(*value, stored)
                } else {
                    true
                }
            }
            // Note: `{:foo}` tests that state contains some binding for `foo`,
            // not that state contains the binding `foo => foo`.
            TestStmt::Set(key) => state.contains_key(&key),
            TestStmt::Unset(key) => !state.contains_key(&key),
        }
    }

    fn eval(&self, stmt: &EvalStmt, state: &mut State) {
        match stmt {
            EvalStmt::Key(key, Token::Lit(sym)) => {
                let _ = state.insert(*key, vector![sym.into()]);
            }
            EvalStmt::Key(key, Token::Plus) => {
                let _ = state.insert(*key, vector![OutputSym::Plus]);
            }
            EvalStmt::Key(key, Token::Var(sym)) => {
                let sentence = self.generate_non_unique_from_start(*sym, state);
                let _ = state.insert(*key, sentence);
            }
            EvalStmt::Key(_, Token::Meta(_)) => {
                unimplemented!("What would this even mean?");
            }
            EvalStmt::Key(key, Token::Scoped(sentence)) => {
                let generated =
                    self.generate_non_unique_from_sentence(sentence.clone(), &mut state.clone());
                let _ = state.insert(*key, generated);
            }
            EvalStmt::Set(key) => {
                let _ = state.insert(*key, vector![OutputSym::Sym(*key)]);
            }
            EvalStmt::Unset(key) => {
                if state.get(key) != None {
                    state.remove(key);
                }
            }
        }
    }

    fn intern(&self, text: &str) -> Sym {
        self.symbol_pool.borrow_mut().get_or_intern(text)
    }

    fn choose_rule(&self, sym: Sym, state: &State) -> Option<&Rule> {
        // Collect all rules that *could* expand `token`.
        let mut possibilities = self
            .grammar
            .rules
            .iter()
            .filter(|rule| rule.head == sym)
            // Theoretically, we're performing this filter down in the `while` loop below.
            // .filter(|rule| rule.pred.iter().all(|stmt| stmt.test(state)))
            .collect::<Vec<_>>();

        // Keep picking random rules until one is found which satisfies it's guard conditions.
        loop {
            if possibilities.is_empty() {
                return None;
            }
            let idx = self.rng.borrow_mut().gen_range(0, possibilities.len());
            let rule = possibilities[idx];
            if rule.test.iter().all(|stmt| self.test(stmt, state)) {
                return Some(rule);
            } else {
                possibilities.swap_remove(idx);
            }
        }
    }

    fn generate_non_unique_from_start(&self, start: Sym, state: &mut State) -> Vector<OutputSym> {
        let sentence = vector![Token::Var(start)];
        self.generate_non_unique_from_sentence(sentence, state)
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
    ) -> Vector<OutputSym> {
        let mut new_sentence: Vector<OutputSym> = Default::default();

        // For each token, if it's a variable, it needs to be replaced.
        for token in sentence {
            match token {
                Token::Lit(sym) => new_sentence.push_back(sym.into()),
                Token::Plus => new_sentence.push_back(OutputSym::Plus),
                Token::Var(sym) => {
                    // more_to_do = true; // We'll have to revisit.

                    // First, check to see if the name is in the state map.
                    // If `sym` is bound in the state map, convert it's
                    // `Vector` of `OutputSym`s into `Token`s and append
                    // them onto `new_sentence.
                    // If it's not in there, search the grammar's rules
                    // via `self.choose_rule`.
                    // Finally, if it's in neither, panic.
                    let tokens_to_add: Vector<OutputSym> = state
                        .get(&sym)
                        .map(Clone::clone)
                        .or_else(|| {
                            let rule = self.choose_rule(sym, state)?;
                            let next_sentence = rule.body.clone();
                            Some(self.generate_non_unique_from_sentence(next_sentence, state))
                        })
                        .unwrap_or_else(|| {
                            panic!(
                                "oops, `{}` does not match any known rule!",
                                self.symbol_pool.borrow().resolve(sym).unwrap()
                            );
                        });

                    new_sentence.append(tokens_to_add);
                }
                Token::Meta(stmts) => {
                    for stmt in &stmts {
                        self.eval(stmt, state);
                    }
                }
                Token::Scoped(sentence) => {
                    self.generate_non_unique_from_sentence(sentence.clone(), &mut state.clone())
                        .into_iter()
                        .for_each(|sym| new_sentence.push_back(sym.into()));
                }
            }
        }

        new_sentence
    }

    /// Takes in a iterator of either `Sym`s or `Plus`es. Before every `Sym`,
    /// insert a space character. If there is a `Plus` before a `Sym`, don't
    /// insert the space. If it's at the beginning of the iterator, don't insert
    /// a space.
    fn join_symbols<'it>(&self, syms: impl Iterator<Item = &'it OutputSym> + 'it) -> String {
        let (lower_bound, _upper) = syms.size_hint();
        let mut text = String::with_capacity(lower_bound);
        let mut add_joining_space = false;

        for sym in syms {
            match sym {
                OutputSym::Sym(sym) => {
                    let pool_ref = self.symbol_pool.borrow();
                    let s = pool_ref.resolve(*sym).unwrap();
                    if add_joining_space {
                        text.push(' ');
                    }
                    text.push_str(s);
                    add_joining_space = true;
                }
                OutputSym::Plus => add_joining_space = false,
            }
        }
        text
    }

    pub fn generate(&mut self) -> Option<String> {
        let start = self.symbol_pool.borrow_mut().get_or_intern("start");
        let mut trials = 0;
        loop {
            let mut state = im::HashMap::new();
            let sentence = self.generate_non_unique_from_start(start, &mut state);
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

        let symbol_pool = make_symbol_pool();
        let parse_res = crate::parse::parse_source(&src, symbol_pool.clone());

        let grammar = match parse_res {
            Ok((_, grammar)) => grammar,
            Err(nom::Err::Error((rest, e))) => {
                eprintln!("Could not parse source text! Got error: {:?}", e);
                eprintln!("Parsed up until this point:```\n{}\n```", rest);
                std::process::exit(-1);
            }
            _ => unimplemented!(),
        };

        Ok(Self::new(grammar, symbol_pool))
    }
}
