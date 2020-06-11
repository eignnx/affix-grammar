use crate::fault::{DynamicErr, DynamicRes};
use im::Vector;
use internship::IStr;
use std::collections::BTreeMap;
use std::fmt;

/// Can appear in a case-analysis in the body of a rule.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Pattern {
    Star,
    Variant(Abbr<DataVariant>),
    Variable(DataVariable),
}

/// The values or variables passed to a rule when it is referenced (called).
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Argument {
    Variant(Abbr<DataVariant>),
    Variable(DataVariable),
}

/// The name of a rule.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct RuleName(pub IStr);

impl AsRef<str> for RuleName {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl fmt::Display for RuleName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_ref())
    }
}

/// A variable that represents a data variant.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct DataVariable(pub Abbr<IStr>, pub IStr);

impl AsRef<str> for DataVariable {
    fn as_ref(&self) -> &str {
        let Self(Abbr(name), _number) = self;
        name.as_ref()
    }
}

impl fmt::Display for DataVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(Abbr(name), number) = self;
        write!(f, "{}{}", name, number)
    }
}

/// The name of a data-type.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct DataName(pub IStr);

impl AsRef<str> for DataName {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
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
/// # use libaffix::parser::syntax::Abbr;
/// assert!(abbreviates(&Abbr::new("G"), "Gender"));
/// assert!(abbreviates(&Abbr::new("Gend"), "Gender"));
/// assert!(abbreviates(&Abbr::new("Gndr"), "Gender"));
/// assert!(abbreviates(&Abbr::new("Gender"), "Gender"));
/// assert!(!abbreviates(&Abbr::new("Genderific"), "Gender"));
///
/// assert!(!abbreviates(&Abbr::new(""), "Whatever"));
///
/// assert!(abbreviates(&Abbr::new("A"), "Argument"));
/// assert!(abbreviates(&Abbr::new("Arg"), "Argument"));
/// assert!(abbreviates(&Abbr::new("Art"), "Argument"));
///
/// assert!(abbreviates(&Abbr::new("A"), "Archetype"));
/// assert!(abbreviates(&Abbr::new("Arch"), "Archetype"));
/// assert!(abbreviates(&Abbr::new("Art"), "Archetype"));
///
/// assert!(abbreviates(&Abbr::new("n"), "neutral"));
/// assert!(abbreviates(&Abbr::new("neut"), "neutral"));
/// assert!(abbreviates(&Abbr::new("ntrl"), "neutral"));
///
/// assert!(!abbreviates(&Abbr::new("cow"), "horse"));
///
/// assert!(abbreviates(&Abbr::new("Nat"), "NaturalNumber"));
/// assert!(abbreviates(&Abbr::new("Num"), "NaturalNumber"));
/// assert!(abbreviates(&Abbr::new("NN"), "NaturalNumber"));
/// assert!(!abbreviates(&Abbr::new("NNN"), "NaturalNumber"));
/// ```
pub fn abbreviates(abbr: &Abbr<impl AsRef<str>>, src: impl AsRef<str>) -> bool {
    let Abbr(abbr) = abbr;
    let mut abbr = abbr.as_ref();
    if abbr.is_empty() {
        return false;
    }
    for ch in src.as_ref().chars() {
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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct DataVariant(pub IStr);

impl AsRef<str> for DataVariant {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl fmt::Display for DataVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_ref())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct Abbr<T>(T);

impl<T> Abbr<T> {
    pub fn new(x: T) -> Self {
        Abbr(x)
    }

    pub fn abbreviation(self) -> T {
        let Self(x) = self;
        x
    }

    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> Abbr<U> {
        let Self(x) = self;
        Abbr(f(x))
    }
}

impl<T> fmt::Display for Abbr<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Abbr(x) = self;
        x.fmt(f)
    }
}

/// The "call site" of a rule. Includes variables that should be referenced inside the call.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct RuleRef {
    pub rule: Abbr<RuleName>,
    pub vars: Vec<Argument>,
}

/// The type signature of a rule.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct RuleSig {
    pub name: RuleName,
    pub parameter_types: Vec<Abbr<DataName>>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Token {
    RuleRef(RuleRef),
    StrLit(IStr),
    DataVariable(DataVariable),
    DataVariant(DataVariant),
    Plus,
}

pub type SententialForm = Vector<Token>;

#[derive(Debug, PartialEq, Serialize)]
pub struct DataDecl {
    pub name: DataName,
    pub variants: BTreeMap<DataVariant, Vec<SententialForm>>,
}

#[derive(Debug, Default, Clone, PartialEq, Serialize)]
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

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Case {
    pub guard: Guard,
    pub alternatives: Vec<SententialForm>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct RuleDecl {
    pub signature: RuleSig,
    pub cases: Vec<Case>,
}

#[derive(Debug, Default, PartialEq, Serialize)]
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
        variant: &Abbr<DataVariant>,
    ) -> DynamicRes<(&'grammar DataDecl, &'grammar DataVariant)> {
        // Search through all data declarations for variants that `val` is
        // an abbreviation of. Collect all those variants.
        // TODO: use the "type signature" of the rule to narrow this search.
        let mut canonicalizations = self.data_decls.iter().flat_map(|decl| {
            decl.variants
                .iter()
                .filter_map(move |(other_variant, _reprs)| {
                    let DataVariant(other_name) = other_variant;
                    if abbreviates(variant, other_name) {
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
                symbol: variant.to_string(),
            })?;

        // If there's more than one possibility, that's an ambiguity.
        if let Some(second) = canonicalizations.next() {
            let (decl1, DataVariant(possibility1)) = first;
            let (decl2, DataVariant(possibility2)) = second;
            return Err(DynamicErr::AmbiguousSymbol {
                symbol: variant.to_string(),
                possibility1: format!("{}::{}", decl1.name.as_ref(), possibility1),
                possibility2: format!("{}::{}", decl2.name.as_ref(), possibility2),
            });
        }

        Ok(first)
    }

    pub fn rule_decl_from_abbr_rule_name<'grammar>(
        &'grammar self,
        abbr_name: &Abbr<RuleName>,
    ) -> DynamicRes<(&'grammar RuleDecl, &'grammar RuleName)> {
        let mut possibilities = self
            .rule_decls
            .iter()
            .filter(|decl| abbreviates(abbr_name, &decl.signature.name));

        let first = possibilities
            .next()
            .ok_or_else(|| DynamicErr::UnboundRuleName {
                rule_name: abbr_name.to_string(),
            })?;

        if let Some(second) = possibilities.next() {
            let possibility1 = first.signature.name.to_string();
            let possibility2 = second.signature.name.to_string();
            return Err(DynamicErr::AmbiguousSymbol {
                symbol: abbr_name.to_string(),
                possibility1,
                possibility2,
            });
        }

        let rule_decl = first;
        Ok((rule_decl, &rule_decl.signature.name))
    }
}
