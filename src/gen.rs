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

impl From<Sym> for OutputSym {
    fn from(sym: Sym) -> Self {
        Self::Sym(sym)
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

    fn eval(&self, stmt: &EvalStmt, state: &mut State) -> Option<Vector<OutputSym>> {
        match stmt {
            EvalStmt::Key(key, value) => match value {
                Token::Lit(sym) => {
                    let _ = state.insert(*key, vector![OutputSym::Sym(*sym)]);
                    None
                }
                Token::Plus => None, // TODO: Could we output the Plus to eat a space outside?
                Token::Var(sym) => {
                    let sentence = self.generate_non_unique(*sym, state);
                    let _ = state.insert(*key, sentence);
                    None
                }
                Token::Meta(stmts) => {
                    let mut last = None;
                    for stmt in stmts {
                        last = self.eval(stmt, state);
                    }
                    last
                }
            },
            EvalStmt::Set(key) => {
                let _ = state.insert(*key, vector![OutputSym::Sym(*key)]);
                None
            }
            EvalStmt::Unset(key) => {
                if state.get(key) != None {
                    state.remove(key);
                }
                None
            }
            EvalStmt::Lookup(key) => state.get(&key).map(|x| x.clone()).or_else(|| {
                let sentence = self.generate_non_unique(*key, state);
                Some(sentence)
            }),
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

    /// Psuedo-code:
    /// Start with the start symbol. Call this `sentence`.
    /// Repeat until there are no non-terminals in `sentence`:
    ///     For each token in the sentence:
    ///         If it's a literal, keep it.
    ///         If it's a non-terminal:
    ///             Collect each rule that the non-terminal matches against.
    ///             Select one of those rules at random.
    ///             Append it's body onto the new sentence.
    fn generate_non_unique(&self, start: Sym, state: &mut State) -> Vector<OutputSym> {
        let mut sentence = vector![Token::Var(start)];
        let mut more_todo = true;

        while more_todo {
            more_todo = false;
            let mut new_sentence = Vector::new();

            // For each token, if it's a variable, it needs to be replaced.
            for token in sentence.clone() {
                match token {
                    Token::Lit(_) => new_sentence.push_back(token.clone()),
                    Token::Meta(stmts) => {
                        for stmt in &stmts {
                            if let Some(sentence) = self.eval(stmt, state) {
                                for output_sym in sentence {
                                    new_sentence.push_back(output_sym.into());
                                }
                            }
                        }
                    }
                    Token::Plus => new_sentence.push_back(Token::Plus),
                    Token::Var(sym) => {
                        more_todo = true; // We'll have to revisit.
                        match self.choose_rule(sym, state) {
                            Some(rule) => {
                                new_sentence.append(rule.body.clone());
                            }
                            None => {
                                panic!(
                                    "oops, `{}` does not match any known rule!",
                                    self.symbol_pool.borrow().resolve(sym).unwrap()
                                );
                            }
                        }
                    }
                }
                sentence = new_sentence.clone();
            }
        }

        // Ensure all are `Sym`s, convert from `Token`s.
        sentence
            .into_iter()
            .map(|tok| match tok {
                Token::Lit(sym) => OutputSym::Sym(sym),
                Token::Var(s) => panic!(
                    "Still non-terminal {:?} left in final sentence!",
                    self.symbol_pool
                        .borrow()
                        .resolve(s)
                        .expect("able to un-intern Sym")
                ),
                Token::Meta(_) => panic!("Still some meta-statement left in final sentence!"),
                Token::Plus => OutputSym::Plus,
            })
            .collect()
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
            let sentence = self.generate_non_unique(start, &mut state);
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
