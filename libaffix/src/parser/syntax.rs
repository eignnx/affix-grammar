use crate::fault::{SemanticErr, SemanticRes};
use im::Vector;
use internship::IStr;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug, Default, PartialEq, Serialize, Clone)]
pub struct ParsedGrammar {
    pub data_decls: Vec<DataDecl>,
    pub rule_decls: Vec<RuleDecl>,
}

impl ParsedGrammar {
    /// Searches the grammar's `DataDecl`s for one that matches an abbreviated
    /// `DataName`.
    pub fn data_decl_from_abbr_data_name<'grammar>(
        &'grammar self,
        abbr_name: &Abbr<DataName>,
    ) -> SemanticRes<&'grammar DataDecl> {
        let abbr_name_ref = abbr_name.abbreviation_ref();

        // First we need to check for identical matches. These should get
        // priority. Like if the abbr is `java` and both `java` and `javascript`
        // are possibilities, assume the user meant `java` not `javascript`
        // cause that's what they wrote.
        {
            let mut identical_possibilities = self
                .data_decls
                .iter()
                .filter(|&decl| abbr_name_ref == &decl.name);

            if let Some(first) = identical_possibilities.next() {
                if let Some(_second) = identical_possibilities.next() {
                    return Err(SemanticErr::DuplicateDeclaration {
                        decl_name: first.name.to_string(),
                    });
                } else {
                    return Ok(first);
                }
            }
        }

        // If no **identical** possibilities are found, then search for
        // unambiguous first letter abbreviations. For example:
        // `disambiguate(m, {man, woman}) => Some(man)`
        // `disambiguate(c, {cat, crustacean}) => None`
        if abbr_name_ref.as_ref().chars().count() == 1 {
            let mut same_first_letter = self.data_decls.iter().filter(|decl| {
                let first_letter = |name: &str| name.chars().next().expect("no empty names");
                first_letter(decl.name.as_ref()) == first_letter(abbr_name_ref.as_ref())
            });

            if let Some(first) = same_first_letter.next() {
                if let Some(second) = same_first_letter.next() {
                    return Err(SemanticErr::AmbiguousSymbol {
                        symbol: abbr_name.to_string(),
                        possibility1: first.name.to_string(),
                        possibility2: second.name.to_string(),
                    });
                } else {
                    return Ok(first);
                }
            }
        }

        // Otherwise, use the `abbreviates` function to find a match.
        {
            let mut matches = self
                .data_decls
                .iter()
                .filter(|decl| decl.name.matches_abbreviation(abbr_name));

            let first = matches.next().ok_or_else(|| SemanticErr::UnboundSymbol {
                symbol: abbr_name.to_string(),
            })?;

            if let Some(second) = matches.next() {
                let DataName(fst) = &first.name;
                let DataName(snd) = &second.name;
                return Err(SemanticErr::AmbiguousSymbol {
                    symbol: abbr_name.to_string(),
                    possibility1: fst.to_string(),
                    possibility2: snd.to_string(),
                });
            }

            Ok(first)
        }
    }

    /// Given a `DataVariable`, this function will perform a lookup in the
    /// grammar and return the `DataDecl` that the variable refers to. Fails if
    /// no `DataDecl` matches the variable, or if the variable is ambiguous and
    /// could refer to multiple `DataDecl`s.
    pub fn data_decl_from_abbr_variable<'grammar>(
        &'grammar self,
        DataVariable(name, _num): &DataVariable,
    ) -> SemanticRes<&'grammar DataDecl> {
        self.data_decl_from_abbr_data_name(name)
    }

    /// Searches through **all** data variant values in the grammar for a
    /// matching `DataVariant`. Note: you probably don't wanna use this method.
    pub fn data_decl_from_untyped_abbr_variant<'grammar>(
        &'grammar self,
        variant: &Abbr<DataVariant>,
    ) -> SemanticRes<(&'grammar DataDecl, &'grammar DataVariant)> {
        let abbr_name_ref = variant.abbreviation_ref();

        let canonicalizations = self.data_decls.iter().flat_map(|decl| {
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

        // First we need to check for identical matches. These should get
        // priority. Like if the abbr is `java` and both `java` and `javascript`
        // are possibilities, assume the user meant `java` not `javascript`
        // cause that's what they wrote.
        {
            let mut identical_possibilities = canonicalizations
                .clone()
                .filter(|(_decl, other_variant)| &abbr_name_ref == other_variant);

            if let Some(first) = identical_possibilities.next() {
                if let Some(second) = identical_possibilities.next() {
                    let (decl1, DataVariant(possibility1)) = first;
                    let (decl2, DataVariant(possibility2)) = second;
                    return Err(SemanticErr::AmbiguousSymbol {
                        symbol: variant.to_string(),
                        possibility1: format!("{}::{}", decl1.name.as_ref(), possibility1),
                        possibility2: format!("{}::{}", decl2.name.as_ref(), possibility2),
                    });
                } else {
                    return Ok(first);
                }
            }
        }

        // If no **identical** possibilities are found, then search for
        // unambiguous first letter abbreviations. For example:
        // `disambiguate(m, {man, woman}) => Some(man)`
        // `disambiguate(c, {cat, crustacean}) => None`
        if abbr_name_ref.as_ref().chars().count() == 1 {
            let mut same_first_letter =
                canonicalizations.clone().filter(|(_decl, other_variant)| {
                    let first_letter = |name: &str| name.chars().next().expect("no empty names");
                    first_letter(other_variant.as_ref()) == first_letter(abbr_name_ref.as_ref())
                });

            if let Some(first) = same_first_letter.next() {
                if let Some(second) = same_first_letter.next() {
                    let (decl1, DataVariant(possibility1)) = first;
                    let (decl2, DataVariant(possibility2)) = second;
                    return Err(SemanticErr::AmbiguousSymbol {
                        symbol: variant.to_string(),
                        possibility1: format!("{}::{}", decl1.name.as_ref(), possibility1),
                        possibility2: format!("{}::{}", decl2.name.as_ref(), possibility2),
                    });
                } else {
                    return Ok(first);
                }
            }
        }

        // Otherwise, use the `abbreviates` function to find a match.
        {
            let mut matches = canonicalizations
                .filter(|(_decl, other_variant)| abbreviates(variant, other_variant));

            let first = matches.next().ok_or_else(|| SemanticErr::UnboundSymbol {
                symbol: variant.to_string(),
            })?;

            if let Some(second) = matches.next() {
                let (decl1, DataVariant(possibility1)) = first;
                let (decl2, DataVariant(possibility2)) = second;
                return Err(SemanticErr::AmbiguousSymbol {
                    symbol: variant.to_string(),
                    possibility1: format!("{}::{}", decl1.name.as_ref(), possibility1),
                    possibility2: format!("{}::{}", decl2.name.as_ref(), possibility2),
                });
            }

            Ok(first)
        }
    }

    pub fn rule_decl_from_abbr_rule_name<'grammar>(
        &'grammar self,
        abbr_name: &Abbr<RuleName>,
    ) -> SemanticRes<(&'grammar RuleDecl, &'grammar RuleName)> {
        // First we need to check for identical matches. These should get
        // priority. Like if the abbr is `java` and both `java` and `javascript`
        // are possibilities, assume the user meant `java` cause that's what
        // they wrote.
        {
            let mut identical_possibilities = self
                .rule_decls
                .iter()
                .filter(|decl| abbr_name.abbreviation_ref() == &decl.signature.name);

            if let Some(first) = identical_possibilities.next() {
                if let Some(_second) = identical_possibilities.next() {
                    return Err(SemanticErr::DuplicateDeclaration {
                        decl_name: first.signature.name.to_string(),
                    });
                } else {
                    let rule_decl = first;
                    return Ok((rule_decl, &rule_decl.signature.name));
                }
            }
        }

        // If no **identical** possibilities are found, then search for
        // unambiguous first letter abbreviations. For example:
        // `disambiguate(m, {man, woman}) => Some(man)`
        // `disambiguate(c, {cat, crustacean}) => None`
        if abbr_name.abbreviation_ref().as_ref().len() == 1 {
            let first_letter = |name: &RuleName| -> char {
                name.as_ref().chars().next().expect("no empty rule names")
            };

            let mut same_first_letter = self.rule_decls.iter().filter(|decl| {
                first_letter(&decl.signature.name) == first_letter(abbr_name.abbreviation_ref())
            });

            if let Some(first) = same_first_letter.next() {
                if let Some(second) = same_first_letter.next() {
                    return Err(SemanticErr::AmbiguousSymbol {
                        symbol: abbr_name.to_string(),
                        possibility1: first.signature.name.to_string(),
                        possibility2: second.signature.name.to_string(),
                    });
                } else {
                    let rule_decl = first;
                    return Ok((rule_decl, &rule_decl.signature.name));
                }
            }
        }

        // Otherwise, use the `abbreviates` fn to search for near matches.
        {
            let mut possibilities = self
                .rule_decls
                .iter()
                .filter(|decl| abbreviates(abbr_name, &decl.signature.name));

            let first = possibilities
                .next()
                .ok_or_else(|| SemanticErr::UnboundRuleName {
                    rule_name: abbr_name.to_string(),
                })?;

            if let Some(second) = possibilities.next() {
                let possibility1 = first.signature.name.to_string();
                let possibility2 = second.signature.name.to_string();
                return Err(SemanticErr::AmbiguousSymbol {
                    symbol: abbr_name.to_string(),
                    possibility1,
                    possibility2,
                });
            }

            let rule_decl = first;
            Ok((rule_decl, &rule_decl.signature.name))
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct DataDecl {
    pub name: DataName,
    pub variants: BTreeMap<DataVariant, Vec<SententialForm>>,
}

impl DataDecl {
    /// Allows an `Abbr<DataVariant>` to be resolved to an unabbreviated
    /// `DataVariant`. There are two error conditions:
    ///     1. If no `DataVariant` in the `self` matches the abbreviated
    ///        argument, then `Err(None)` is returned.
    ///     2. If more than one `DataVariant` in `self` matches the abbreviated
    ///        argument, then both matching `DataVariant`s will be returned as
    ///        a pair, i.e. like: `Err(Some((1st_match, 2nd_match)))`.
    pub fn lookup_variant(&self, abbr_variant: &Abbr<DataVariant>) -> SemanticRes<&DataVariant> {
        // let mut found: Option<&DataVariant> = None;

        // for variant in self.variants.keys() {
        //     if abbreviates(abbr_variant, variant) {
        //         if let Some(prev_match) = found {
        //             return Err(SemanticErr::AmbiguousSymbol {
        //                 symbol: abbr_variant.to_string(),
        //                 possibility1: format!("{}::{}", self.name, prev_match),
        //                 possibility2: format!("{}::{}", self.name, variant),
        //             });
        //         } else {
        //             found = Some(variant);
        //         }
        //     }
        // }

        // found.ok_or(SemanticErr::UnboundSymbol {
        //     symbol: abbr_variant.to_string(),
        // })
        let abbr_name_ref = abbr_variant.abbreviation_ref();

        // First we need to check for identical matches. These should get
        // priority. Like if the abbr is `java` and both `java` and `javascript`
        // are possibilities, assume the user meant `java` not `javascript`
        // cause that's what they wrote.
        {
            let mut identical_possibilities = self
                .variants
                .keys()
                .filter(|other_variant| &abbr_name_ref == other_variant);

            if let Some(first) = identical_possibilities.next() {
                if let Some(second) = identical_possibilities.next() {
                    let DataVariant(possibility1) = first;
                    let DataVariant(possibility2) = second;
                    return Err(SemanticErr::AmbiguousSymbol {
                        symbol: abbr_variant.to_string(),
                        possibility1: format!("{}::{}", self.name.as_ref(), possibility1),
                        possibility2: format!("{}::{}", self.name.as_ref(), possibility2),
                    });
                } else {
                    return Ok(first);
                }
            }
        }

        // If no **identical** possibilities are found, then search for
        // unambiguous first letter abbreviations. For example:
        // `disambiguate(m, {man, woman}) => Some(man)`
        // `disambiguate(c, {cat, crustacean}) => None`
        if abbr_name_ref.as_ref().chars().count() == 1 {
            let mut same_first_letter = self.variants.keys().filter(|other_variant| {
                let first_letter = |name: &str| name.chars().next().expect("no empty names");
                first_letter(other_variant.as_ref()) == first_letter(abbr_name_ref.as_ref())
            });

            if let Some(first) = same_first_letter.next() {
                if let Some(second) = same_first_letter.next() {
                    let DataVariant(possibility1) = first;
                    let DataVariant(possibility2) = second;
                    return Err(SemanticErr::AmbiguousSymbol {
                        symbol: abbr_variant.to_string(),
                        possibility1: format!("{}::{}", self.name.as_ref(), possibility1),
                        possibility2: format!("{}::{}", self.name.as_ref(), possibility2),
                    });
                } else {
                    return Ok(first);
                }
            }
        }

        // Otherwise, use the `abbreviates` function to find a match.
        {
            let mut matches = self
                .variants
                .keys()
                .filter(|other_variant| abbreviates(abbr_variant, other_variant));

            let first = matches.next().ok_or_else(|| SemanticErr::UnboundSymbol {
                symbol: abbr_variant.to_string(),
            })?;

            if let Some(second) = matches.next() {
                let DataVariant(possibility1) = first;
                let DataVariant(possibility2) = second;
                return Err(SemanticErr::AmbiguousSymbol {
                    symbol: abbr_variant.to_string(),
                    possibility1: format!("{}::{}", self.name.as_ref(), possibility1),
                    possibility2: format!("{}::{}", self.name.as_ref(), possibility2),
                });
            }

            Ok(first)
        }
    }
}

/// The name of a data-type.
#[derive(Debug, Clone, PartialEq, Serialize, Eq, PartialOrd, Ord, Hash)]
pub struct DataName(pub IStr);

impl From<&str> for DataName {
    fn from(name: &str) -> Self {
        Self(IStr::new(name))
    }
}

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

impl DataName {
    /// Performs an equality check with a `DataVariable` since a `DataVariable`
    /// can be abbreviated.
    pub fn matches_abbreviation(&self, abbr: &Abbr<DataName>) -> bool {
        let DataName(data_name) = self;
        abbreviates(abbr, data_name)
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

/// The name of a variant of a data-type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct DataVariant(pub IStr);

impl From<&str> for DataVariant {
    fn from(name: &str) -> Self {
        Self(IStr::new(name))
    }
}

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

/// A sequence of `Token`s. This could be either `DataVariable`s, `DataVariant`s,
/// `RuleRef`s, (etc), or raw text literals.
pub type SententialForm = Vector<Token>;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Token {
    RuleRef(RuleRef),
    StrLit(IStr),
    DataVariable(Stringification<DataVariable>),
    DataVariant(Stringification<Abbr<DataVariant>>),
    Plus,
}

/// The "call site" of a rule. Includes variables that should be referenced inside the call.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct RuleRef {
    pub rule: Abbr<RuleName>,
    pub args: Vec<Argument>,
}

impl fmt::Display for RuleRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.rule)?;

        for arg in &self.args {
            write!(f, ".{}", arg)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Stringification<T>(pub(crate) T);

impl<T> Stringification<T> {
    pub fn inner(self) -> T {
        let Stringification(inner) = self;
        inner
    }

    pub fn inner_ref(&self) -> &T {
        let Stringification(inner) = self;
        inner
    }
}

/// A variable that represents a data variant.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct DataVariable(pub Abbr<DataName>, pub IStr);

impl From<(&str, &str)> for DataVariable {
    fn from((name, number): (&str, &str)) -> Self {
        Self(Abbr::new(name.into()), IStr::new(number))
    }
}

impl fmt::Display for DataVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(Abbr(name), number) = self;
        write!(f, "{}{}", name, number)
    }
}

/// The values or variables passed to a rule when it is referenced (called).
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Argument {
    Variant(Abbr<DataVariant>),
    Variable(DataVariable),
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Variant(v) => write!(f, "{}", v),
            Self::Variable(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct RuleDecl {
    pub signature: RuleSig,
    pub cases: Vec<Case>,
}

/// The type signature of a rule.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct RuleSig {
    pub name: RuleName,
    pub parameter_types: Vec<Abbr<DataName>>,
}

/// The name of a rule.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct RuleName(pub IStr);

impl From<&str> for RuleName {
    fn from(name: &str) -> Self {
        Self(IStr::new(name))
    }
}

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

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Case {
    pub guard: Guard,
    pub alternatives: Vec<SententialForm>,
}

#[derive(Debug, Default, Clone, PartialEq, Serialize)]
pub struct Guard {
    // This field needs to be cheaply copiable because several parsers pass
    // guards around and need to clone them.
    pub requirements: Vector<Pattern>,
}

impl fmt::Display for Guard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut reqs = self.requirements.iter();

        if let Some(first) = reqs.next() {
            write!(f, "{}", first)?;
            for pattern in reqs {
                write!(f, ".{}", pattern)?;
            }
        }

        Ok(())
    }
}

impl Guard {
    pub(crate) fn append(&mut self, other: &Self) {
        for req in &other.requirements {
            self.requirements.push_back(req.clone());
        }
    }
}

/// Can appear in a case-analysis in the body of a rule.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Pattern {
    Star,
    Variant(Abbr<DataVariant>),
    Variable(DataVariable),
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Star => write!(f, "*"),
            Pattern::Variant(v) => write!(f, "{}", v),
            Pattern::Variable(v) => write!(f, "{}", v),
        }
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

    pub fn abbreviation_ref(&self) -> &T {
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
