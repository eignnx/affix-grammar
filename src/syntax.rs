use im::vector;
use im::Vector;
use rand::seq::SliceRandom;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use string_interner::{StringInterner, Sym};

#[derive(Debug, Clone, PartialEq)]
pub enum Token<T = Sym> {
    Var(T),
    Lit(T),
}

#[derive(Debug)]
pub struct Rule {
    pub(crate) head: Sym,
    pub(crate) pred: Vector<(Sym, Sym)>,
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
        _state: &mut HashMap<Sym, Sym>,
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
                    Token::Var(sym) => {
                        more_todo = true;
                        // Collect all rules that *could* expand `token`.
                        let possiblilties = self
                            .rules
                            .iter()
                            .filter(|rule| rule.head == sym)
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
    dbg!(g.generate(syms, &mut thread_rng(), &mut HashMap::new()));
}
