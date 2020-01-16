use im::vector;
use im::Vector;
use rand::seq::SliceRandom;
use std::{cell::RefCell, rc::Rc};
use string_interner::{StringInterner, Sym};

#[derive(Debug, Clone, PartialEq)]
pub enum Token<S: Clone = Sym> {
    Var(S),
    Lit(S),
    Meta(Vector<Stmt<S>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<S = Sym> {
    Key(S, S),
    NotKey(S, S),
    Lookup(S),
}

type State = im::HashMap<Sym, Sym>;

impl Stmt {
    fn test(&self, state: &State) -> bool {
        match self {
            Self::Key(key, value) => state.get(&key) == Some(&value),
            Self::NotKey(key, value) => state.get(&key) != Some(&value),
            Self::Lookup(key) => state.contains_key(&key),
        }
    }

    fn eval(&self, state: &mut State) -> Option<Sym> {
        match self {
            Self::Key(key, value) => {
                let _ = state.insert(key.clone(), value.clone());
                None
            }
            Self::NotKey(key, value) => {
                if state.get(key) == Some(value) {
                    state.remove(key);
                }
                None
            }
            Self::Lookup(key) => state.get(&key).map(|x| x.clone()),
        }
    }
}

#[derive(Debug)]
pub struct Rule {
    pub(crate) head: Sym,
    pub(crate) pred: Vector<Stmt>,
    pub(crate) body: Vector<Token>,
}

#[derive(Debug)]
pub struct Grammar {
    pub(crate) rules: Vec<Rule>,
}

impl Grammar {
    /// Psuedo-code:
    /// Start with the start symbol. Call this `sentence`.
    /// Repeat until there are no non-terminals in `sentence`:
    ///     For each token in the sentence:
    ///         If it's a literal, keep it.
    ///         If it's a non-terminal:
    ///             Collect each rule that the non-terminal matches against.
    ///             Select one of those rules at random.
    ///             Append it's body onto the new sentence.
    pub fn generate(
        &self,
        syms: Rc<RefCell<StringInterner<Sym>>>,
        rng: &mut impl rand::Rng,
        state: &mut State,
    ) -> String {
        let start = syms.borrow_mut().get_or_intern("start");
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
                            if let Some(sym) = stmt.eval(state) {
                                // TODO: Should this push `Token`s rather than `Sym`s?
                                new_sentence.push_back(Token::Lit(sym.clone()));
                            }
                        }
                    }
                    Token::Var(sym) => {
                        more_todo = true;
                        // Collect all rules that *could* expand `token`.
                        let possiblilties = self
                            .rules
                            .iter()
                            .filter(|rule| rule.head == sym)
                            .filter(|rule| rule.pred.iter().all(|stmt| stmt.test(state)))
                            .collect::<Vec<_>>();
                        // Randomly pick a rule.
                        if let Some(rule) = possiblilties.choose(rng) {
                            new_sentence.append(rule.body.clone());
                        } else {
                            panic!(
                                "oops, `{:?}` is not a known rule head!",
                                syms.borrow().resolve(sym)
                            );
                        }
                    }
                }
                sentence = new_sentence.clone();
            }
        }

        let mut buf = String::new();
        for token in sentence {
            match token {
                Token::Lit(s) => {
                    let syms_ref = syms.borrow();
                    let lit = syms_ref.resolve(s).expect("able to un-intern Sym");
                    buf.push_str(lit);
                }
                Token::Var(s) => panic!(
                    "Still non-terminal {:?} left in final sentence!",
                    syms.borrow().resolve(s).expect("able to un-intern Sym")
                ),
                Token::Meta(_) => panic!("Still some meta-statement left in final sentence!"),
            }
        }
        buf
    }
}

#[test]
fn test_construction() {
    use rand::thread_rng;
    use string_interner::StringInterner;

    let syms = Rc::new(RefCell::new(StringInterner::default()));
    let sym = |s: &str| syms.borrow_mut().get_or_intern(s);

    let g = Grammar {
        rules: vec![
            Rule {
                head: sym("start"),
                pred: vector![],
                body: vector![Token::Lit(sym("la")), Token::Var(sym("nn")),],
            },
            Rule {
                head: sym("nn"),
                pred: vector![(sym("gender"), sym("f"))],
                body: vector![Token::Lit(sym("femme"))],
            },
        ],
    };
    dbg!(g.generate(syms, &mut thread_rng(), &mut im::HashMap::new()));
}
