use im::Vector;
use internship::IStr;
use std::collections::HashSet;

/// Can appear in a case-analysis in the body of a rule.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Star,
    Variant(DataVariant),
}

/// The values or variables passed to a rule when it is referenced (called).
#[derive(Debug, Clone, PartialEq)]
pub enum Argument {
    Variant(DataVariant),
    Variable(DataVariable),
}

/// The name of a rule.
#[derive(Debug, Clone, PartialEq)]
pub struct RuleName(pub IStr);

/// A variable that represents a data variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DataVariable(pub IStr);

/// The name of a data-type.
#[derive(Debug, Clone, PartialEq)]
pub struct DataName(pub IStr);

impl DataName {
    /// Performs an equality check with a `DataVariable` since a `DataVariable`
    /// can have a numeric suffix to distinguish it.
    pub fn matches_variable(&self, variable: &DataVariable) -> bool {
        let DataVariable(var_txt) = variable;
        let var_without_nums = var_txt.as_str().trim_end_matches(char::is_numeric);
        let DataName(name) = self;
        name.as_str().starts_with(var_without_nums)
    }
}

/// The name of a variant of a data-type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DataVariant(pub IStr);

/// The "call site" of a rule. Includes variables that should be referenced inside the call.
#[derive(Debug, Clone, PartialEq)]
pub struct RuleRef {
    pub rule: RuleName,
    pub vars: Vec<Argument>,
}

/// The type signature of a rule.
#[derive(Debug, Clone, PartialEq)]
pub struct RuleSig {
    pub name: RuleName,
    pub parameter_types: Vec<DataName>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    RuleRef(RuleRef),
    StrLit(IStr),
    Plus,
}

pub type SententialForm = Vector<Token>;

#[derive(Debug, PartialEq)]
pub struct DataDecl {
    pub name: DataName,
    pub variants: HashSet<DataVariant>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Guard {
    pub requirements: Vec<Pattern>,
}

impl Guard {
    pub(crate) fn append(&mut self, other: &Self) {
        for req in &other.requirements {
            self.requirements.push(req.clone());
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct RuleBody {
    pub guard: Guard,
    pub sentential_form: SententialForm,
}

#[derive(Debug, PartialEq)]
pub struct RuleDecl {
    pub signature: RuleSig,
    pub bodies: Vec<RuleBody>,
}

#[derive(Debug, Default, PartialEq)]
pub struct Grammar {
    pub data_decls: Vec<DataDecl>,
    pub rule_decls: Vec<RuleDecl>,
}
