use im::Vector;
use string_interner::Sym;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<S: Clone = Sym> {
    Var(S),
    Lit(S),
    Plus,
    Meta(Vector<EvalStmt<S>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum EvalStmt<S: Clone = Sym> {
    Key(S, Token<S>),
    Set(S),
    Unset(S),
}

#[derive(Debug, Clone)]
pub enum TestStmt<S: Clone = Sym> {
    /// { var: "lit" }
    Key(S, S),
    /// { var! "lit" }
    NotKey(S, S),
    /// { :var }
    Set(S),
    /// { !var }
    Unset(S),
}

#[derive(Debug)]
pub struct Rule {
    pub(crate) head: Sym,
    pub(crate) test: Vector<TestStmt>,
    pub(crate) body: Vector<Token>,
}

#[derive(Debug)]
pub struct Grammar {
    pub(crate) rules: Vec<Rule>,
}
