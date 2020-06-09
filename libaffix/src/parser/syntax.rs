use crate::fault::{DynamicErr, DynamicRes};
use im::Vector;
use internship::IStr;
use std::collections::HashMap;
use std::fmt;

/// Can appear in a case-analysis in the body of a rule.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Star,
    Variant(DataVariant),
    Variable(DataVariable),
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

impl AsRef<str> for RuleName {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl fmt::Display for RuleName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_ref())
    }
}

/// A variable that represents a data variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DataVariable(pub IStr, pub IStr);

impl AsRef<str> for DataVariable {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl fmt::Display for DataVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let DataVariable(name, number) = self;
        write!(f, "{}{}", name, number)
    }
}

/// The name of a data-type.
#[derive(Debug, Clone, PartialEq)]
pub struct DataName(pub IStr);

impl AsRef<str> for DataName {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl fmt::Display for DataName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_ref())
    }
}

/// A predicate function that determines if one word (`abbr`) abbreviates the
/// other (`src`). See doctests for examples.
///
/// ```
/// # use libaffix::parser::syntax::abbreviates;
/// assert!(abbreviates("G", "Gender"));
/// assert!(abbreviates("Gend", "Gender"));
/// assert!(abbreviates("Gndr", "Gender"));
/// assert!(abbreviates("Gender", "Gender"));
/// assert!(!abbreviates("Genderific", "Gender"));
///
/// assert!(!abbreviates("", "Whatever"));
///
/// assert!(abbreviates("A", "Argument"));
/// assert!(abbreviates("Arg", "Argument"));
/// assert!(abbreviates("Art", "Argument"));
///
/// assert!(abbreviates("A", "Archetype"));
/// assert!(abbreviates("Arch", "Archetype"));
/// assert!(abbreviates("Art", "Archetype"));
///
/// assert!(abbreviates("n", "neutral"));
/// assert!(abbreviates("neut", "neutral"));
/// assert!(abbreviates("ntrl", "neutral"));
///
/// assert!(!abbreviates("cow", "horse"));
///
/// assert!(abbreviates("Nat", "NaturalNumber"));
/// assert!(abbreviates("Num", "NaturalNumber"));
/// assert!(abbreviates("NN", "NaturalNumber"));
/// assert!(!abbreviates("NNN", "NaturalNumber"));
/// ```
pub fn abbreviates(mut abbr: &str, src: &str) -> bool {
    if abbr.is_empty() {
        return false;
    }
    for ch in src.chars() {
        if abbr.starts_with(ch) {
            abbr = &abbr[1..];
        }
    }
    abbr.is_empty()
}

impl DataName {
    /// Performs an equality check with a `DataVariable` since a `DataVariable`
    /// can be abbreviated.
    pub fn matches_variable(&self, DataVariable(var_name, _number): &DataVariable) -> bool {
        let DataName(data_name) = self;
        abbreviates(var_name, data_name)
    }
}

/// The name of a variant of a data-type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DataVariant(pub IStr);

impl AsRef<str> for DataVariant {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl fmt::Display for DataVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_ref())
    }
}

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
    DataVariable(DataVariable),
    DataVariant(DataVariant),
    Plus,
}

pub type SententialForm = Vector<Token>;

#[derive(Debug, PartialEq)]
pub struct DataDecl {
    pub name: DataName,
    pub variants: HashMap<DataVariant, Vec<SententialForm>>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Guard {
    // This field needs to be cheaply copiable because several parsers pass
    // guards around and need to clone them.
    pub requirements: Vector<Pattern>,
}

impl Guard {
    pub(crate) fn append(&mut self, other: &Self) {
        for req in &other.requirements {
            self.requirements.push_back(req.clone());
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Case {
    pub guard: Guard,
    pub alternatives: Vec<SententialForm>,
}

#[derive(Debug, PartialEq)]
pub struct RuleDecl {
    pub signature: RuleSig,
    pub cases: Vec<Case>,
}

#[derive(Debug, Default, PartialEq)]
pub struct Grammar {
    pub data_decls: Vec<DataDecl>,
    pub rule_decls: Vec<RuleDecl>,
}

impl Grammar {
    /// Given a `DataVariable`, this function will perform a lookup in the
    /// grammar and return the `DataDecl` that the variable refers to. Fails if
    /// no `DataDecl` matches the variable, or if the variable is ambiguous and
    /// could refer to multiple `DataDecl`s.
    pub fn data_decl_from_abbr_variable<'grammar>(
        &'grammar self,
        var: &DataVariable,
    ) -> DynamicRes<&'grammar DataDecl> {
        let mut matches = self
            .data_decls
            .iter()
            .filter(|decl| decl.name.matches_variable(var));

        let DataVariable(name, _num) = var;

        let first = matches.next().ok_or_else(|| DynamicErr::UnboundSymbol {
            symbol: name.to_string(),
        })?;

        if let Some(second) = matches.next() {
            let DataName(fst) = &first.name;
            let DataName(snd) = &second.name;
            return Err(DynamicErr::AmbiguousSymbol {
                symbol: name.to_string(),
                possibility1: fst.to_string(),
                possibility2: snd.to_string(),
            });
        }

        Ok(first)
    }

    pub fn data_decl_from_abbr_variant<'grammar>(
        &'grammar self,
        variant: &DataVariant,
    ) -> DynamicRes<(&'grammar DataDecl, &'grammar DataVariant)> {
        let variant_name = variant.as_ref();

        // Search through all data declarations for variants that `val` is
        // an abbreviation of. Collect all those variants.
        // TODO: use the "type signature" of the rule to narrow this search.
        let mut canonicalizations = self.data_decls.iter().flat_map(|decl| {
            decl.variants
                .iter()
                .filter_map(move |(other_variant, _reprs)| {
                    let DataVariant(other_name) = other_variant;
                    if abbreviates(variant_name, other_name.as_str()) {
                        Some((decl, other_variant))
                    } else {
                        None
                    }
                })
        });

        // Take the first one, and if there are none, that's an unbound
        // symbol error.
        let first = canonicalizations
            .next()
            .ok_or_else(|| DynamicErr::UnboundSymbol {
                symbol: variant_name.to_string(),
            })?;

        // If there's more than one possibility, that's an ambiguity.
        if let Some(second) = canonicalizations.next() {
            let (decl1, DataVariant(possibility1)) = first;
            let (decl2, DataVariant(possibility2)) = second;
            return Err(DynamicErr::AmbiguousSymbol {
                symbol: variant_name.to_string(),
                possibility1: format!("{}::{}", decl1.name.as_ref(), possibility1),
                possibility2: format!("{}::{}", decl2.name.as_ref(), possibility2),
            });
        }

        Ok(first)
    }
}
