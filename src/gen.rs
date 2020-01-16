use crate::syntax::Grammar;
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
    rng: rand::rngs::ThreadRng,
    symbol_pool: Syms,
    seen_sentences: HashSet<im::Vector<Sym>>,
    max_trials: usize,
}

const DEFAULT_MAX_TRIALS: usize = 1000;

impl Generator {
    pub fn new(grammar: Grammar, symbol_pool: Syms) -> Self {
        Self {
            grammar,
            rng: rand::thread_rng(),
            symbol_pool,
            seen_sentences: HashSet::new(),
            max_trials: DEFAULT_MAX_TRIALS,
        }
    }

    pub fn generate(&mut self) -> Option<String> {
        let mut trials = 0;
        loop {
            let mut state: State = im::HashMap::new();
            let sentence =
                self.grammar
                    .generate(self.symbol_pool.clone(), &mut self.rng, &mut state);
            if !self.seen_sentences.contains(&sentence) {
                self.seen_sentences.insert(sentence.clone());
                let mut text = String::with_capacity(sentence.len());
                for sym in sentence {
                    let pool_ref = self.symbol_pool.borrow();
                    let s = pool_ref.resolve(sym).unwrap();
                    text.push_str(s);
                }
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
