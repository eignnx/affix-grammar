use im::Vector;
use string_interner::Sym;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<S: Clone = Sym> {
    /// var_name
    Var(S),

    /// "string literal"
    Lit(S),

    /// +
    Plus,

    /// { stmt_1, stmt_2, ..., stmt_n }
    Meta(Vector<EvalStmt<S>>),

    /// ( sentence )
    Scoped(Vector<Token>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum EvalStmt<S: Clone = Sym> {
    /// { var: token }
    KeyValue(S, Token<S>),

    /// { :var }
    FlagSet(S),

    /// { !var }
    FlagUnset(S),
}

#[derive(Debug, Clone)]
pub enum TestStmt<S: Clone = Sym> {
    /// { var: "lit" }
    KeyValue(S, S),

    /// { var! "lit" }
    NotKeyValue(S, S),

    /// { :var }
    FlagSet(S),

    /// { !var }
    FlagUnset(S),
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
