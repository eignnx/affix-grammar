use im::Vector;
use string_interner::Sym;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<S: Clone = Sym> {
    Var(S),
    Lit(S),
    Meta(Vector<Stmt<S>>),
}

// TODO: use separate data structures for eval and test expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<S: Clone = Sym> {
    Key(S, Token<S>),
    NotKey(S, S), // Should prolly only allow on the test side.
    Set(S),
    Unset(S),
    Lookup(S), // Should prolly only allow on the eval side.
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

#[test]
fn test_construction() {
    use crate::gen::make_symbol_pool;
    use rand::thread_rng;

    let syms = make_symbol_pool();
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
                pred: vector![Stmt::Key(sym("gender"), sym("f"))],
                body: vector![Token::Lit(sym("femme"))],
            },
        ],
    };
    dbg!(g.generate(syms, &mut thread_rng(), &mut im::HashMap::new()));
}
