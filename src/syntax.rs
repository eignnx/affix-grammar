use im::Vector;
use internship::IStr;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<S: Clone = IStr> {
    /// var_name
    Var(S),

    /// "string literal"
    Lit(S),

    /// +
    Plus,

    /// { stmt_1, stmt_2, ..., stmt_n }
    Meta(Vector<EvalStmt<S>>),

    /// ( sentence )
    /// or
    /// ( sentence )[var_1, var_2, ... var_n]
    Scoped(Vector<Token>, Vec<S>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum EvalStmt<S: Clone = IStr> {
    /// { var: token }
    KeyValue(S, Token<S>),

    /// { :var }
    FlagSet(S),

    /// { !var }
    FlagUnset(S),
}

#[derive(Debug, Clone)]
pub enum TestStmt<S: Clone = IStr> {
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
    pub(crate) head: IStr,
    pub(crate) test: Vector<TestStmt>,
    pub(crate) body: Vector<Token>,
}

#[derive(Debug)]
pub struct Grammar {
    pub(crate) rules: Vec<Rule>,
}
