use crate::syntax::{Grammar, Rule, Stmt, Token};
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

pub type State = im::HashMap<Sym, Sym>;

pub type Syms = Rc<RefCell<StringInterner<Sym>>>;

pub fn make_symbol_pool() -> Syms {
    Rc::new(RefCell::new(StringInterner::default()))
}

pub struct Generator {
    grammar: Grammar,
    rng: RefCell<rand::rngs::ThreadRng>,
    symbol_pool: Syms,
    seen_sentences: HashSet<im::Vector<Sym>>,
    pub(crate) max_trials: usize,
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

    fn test(&self, stmt: &Stmt, state: &State) -> bool {
        match stmt {
            Stmt::Key(key, value) => match value {
                Token::Lit(sym) => state.get(&key) == Some(&sym),
                Token::Var(_) => unimplemented!(
                    "this should probably entail PARSING according to the specified rule"
                ),
                Token::Meta(_) => unimplemented!("what would this even mean?"),
            },
            Stmt::NotKey(key, value) => state.get(&key) != Some(&value),
            Stmt::Set(key) => state.contains_key(&key),
            Stmt::Unset(key) => !state.contains_key(&key),
            Stmt::Lookup(_) => {
                unimplemented!("Lookup syntax is not supported in a guard expression!")
            }
        }
    }

    fn eval(&self, stmt: &Stmt, state: &mut State) -> Option<Sym> {
        match stmt {
            Stmt::Key(key, value) => match value {
                Token::Lit(sym) => {
                    let _ = state.insert(*key, *sym);
                    None
                }
                Token::Var(sym) => {
                    let sentence = self.generate_non_unique(*sym, state.clone());
                    let sentence_text = self.join_symbols(sentence.iter());
                    let sentence_sym = self.intern(&sentence_text);
                    let _ = state.insert(*key, sentence_sym);
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
            Stmt::NotKey(key, value) => {
                if state.get(key) == Some(value) {
                    state.remove(key);
                }
                None
            }
            Stmt::Set(key) => {
                let _ = state.insert(key.clone(), key.clone());
                None
            }
            Stmt::Unset(key) => {
                if state.get(key) != None {
                    state.remove(key);
                }
                None
            }
            Stmt::Lookup(key) => state.get(&key).map(|x| x.clone()).or_else(|| {
                let sentence = self.generate_non_unique(*key, state.clone());
                let sentence_text = self.join_symbols(sentence.iter());
                let sentence_sym = self.intern(&sentence_text);
                Some(sentence_sym)
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
            if rule.pred.iter().all(|stmt| self.test(stmt, state)) {
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
    pub fn generate_non_unique(&self, start: Sym, mut state: State) -> Vector<Sym> {
        let mut sentence = vector![Token::Var(start)];
        let mut more_todo = true;

        while more_todo {
            more_todo = false;

            // For each token, if it's a variable, it needs to be replaced.
            let mut new_sentence = Vector::new();

            for token in sentence.clone() {
                match token {
                    Token::Lit(_) => new_sentence.push_back(token.clone()),
                    Token::Meta(stmts) => {
                        for stmt in &stmts {
                            if let Some(sym) = self.eval(stmt, &mut state) {
                                // TODO: Should this push `Token`s rather than `Sym`s?
                                new_sentence.push_back(Token::Lit(sym.clone()));
                            }
                        }
                    }
                    Token::Var(sym) => {
                        more_todo = true; // We'll have to revisit.
                        match self.choose_rule(sym, &state) {
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
                Token::Lit(sym) => sym,
                Token::Var(s) => panic!(
                    "Still non-terminal {:?} left in final sentence!",
                    self.symbol_pool
                        .borrow()
                        .resolve(s)
                        .expect("able to un-intern Sym")
                ),
                Token::Meta(_) => panic!("Still some meta-statement left in final sentence!"),
            })
            .collect()
    }

    fn join_symbols<'it>(&self, syms: impl Iterator<Item = &'it Sym> + 'it) -> String {
        let (lower_bound, _upper) = syms.size_hint();
        let mut text = String::with_capacity(lower_bound);
        for sym in syms {
            let pool_ref = self.symbol_pool.borrow();
            let s = pool_ref.resolve(*sym).unwrap();
            text.push_str(s);
        }
        text
    }

    pub fn generate(&mut self) -> Option<String> {
        let start = self.symbol_pool.borrow_mut().get_or_intern("start");
        let mut trials = 0;
        loop {
            let sentence = self.generate_non_unique(start, im::HashMap::new());
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
