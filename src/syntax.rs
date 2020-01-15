use im::vector;
use im::Vector;
use rand::seq::SliceRandom;
use rand::thread_rng;
use std::collections::HashMap;
use string_interner::{StringInterner, Sym};

#[derive(Debug, Clone)]
pub enum Token {
    Var(Sym),
    Lit(Sym),
}

#[derive(Debug)]
pub struct Rule {
    head: Sym,
    pred: Vector<(Sym, Sym)>,
    body: Vector<Token>,
}

#[derive(Debug)]
pub struct Grammar {
    rules: Vec<Rule>,
}

impl Grammar {
    #[allow(unused)]
    // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
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
        syms: &mut StringInterner<Sym>,
        rng: &mut impl rand::Rng,
        state: &mut HashMap<Sym, Sym>,
    ) -> String {
        let start = syms.get_or_intern("start");
        let mut sentence = vector![Token::Var(start)];
        let mut more_todo = true;

        while more_todo {
            more_todo = false;

            // For each token, if it's a variable, it needs to be replaced.
            let mut new_sentence = Vector::new();

            for token in sentence.clone() {
                dbg!(sentence.clone());
                match token {
                    Token::Lit(_) => new_sentence.push_back(token.clone()),
                    Token::Var(sym) => {
                        more_todo = true;
                        // Collect all rules that *could* expand `token`.
                        let mut possiblilties = self
                            .rules
                            .iter()
                            .filter(|rule| rule.head == sym)
                            .collect::<Vec<_>>();
                        // Randomly pick a rule.
                        if let Some(rule) = possiblilties.choose(rng) {
                            new_sentence.append(rule.body.clone());
                        } else {
                            panic!("oops, `{:?}` is not a known rule head!", syms.resolve(sym));
                        }
                        sentence = new_sentence.clone();
                    }
                }
            }
        }

        sentence
            .iter()
            .map(|tok| match *tok {
                Token::Lit(s) => syms.resolve(s).expect("able to un-intern Sym"),
                Token::Var(s) => panic!(
                    "Still non-terminal {:?} left in final sentence!",
                    syms.resolve(s).expect("able to un-intern Sym")
                ),
            })
            .collect()
    }
}

#[test]
fn test_construction() {
    use string_interner::StringInterner;

    let mut syms = StringInterner::default();
    let mut sym = |s: &str| syms.get_or_intern(s);

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
    dbg!(g.generate(&mut syms, &mut thread_rng(), &mut HashMap::new()));
}
