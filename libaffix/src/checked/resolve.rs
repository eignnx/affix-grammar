//! Walks a `parser::syntax::ParsedGrammar` and transforms it into a `checked::semantics::ResolvedGrammar`. As
//! this is happening, static semantic errors may be thrown.

use crate::fault;
use crate::parser::syntax::{self, Abbr, ParsedGrammar};
use internship::IStr;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};

// Re-export these names.
pub use crate::parser::syntax::{DataName, DataVariant, RuleName};

/// A version of the grammar in which all of the identifiers have been verified
/// as refering -- unambiguously -- to valid `DataDecl`s, `RuleDecl`s, or
/// `DataVariant`s.
pub struct ResolvedGrammar {
    pub data_decls: HashMap<DataName, DataDecl>,
    pub rule_decls: HashMap<RuleName, RuleDecl>,
}

impl Default for ResolvedGrammar {
    fn default() -> Self {
        Self {
            data_decls: Default::default(),
            rule_decls: Default::default(),
        }
    }
}

/// The main translation impl from `syntax::ParsedGrammar` to `semantics::ResolvedGrammar`.
impl std::convert::TryFrom<ParsedGrammar> for ResolvedGrammar {
    type Error = fault::DynamicErr;

    fn try_from(parsed_grammar: ParsedGrammar) -> Result<Self, Self::Error> {
        let (resolved_grammar, _signature_map) = parsed_grammar.try_into()?;
        Ok(resolved_grammar)
    }
}

/// This impl gives back a `SignatureMap` in addition to a `ResolvedGrammar`.
impl std::convert::TryFrom<ParsedGrammar> for (ResolvedGrammar, SignatureMap) {
    type Error = fault::DynamicErr;

    fn try_from(parsed_grammar: ParsedGrammar) -> fault::DynamicRes<Self> {
        let mut new_grammar = ResolvedGrammar::default();

        // First we need to get the unambiguously-typed signatures of all the
        // rules.
        let rule_sigs = validated_rule_signatures(&parsed_grammar)?;

        validate_data_decls(&mut new_grammar, &parsed_grammar, &rule_sigs)?;

        Ok((new_grammar, rule_sigs))
    }
}

/// Validates all the `syntax::DataDecl`s, and adds the validated versions to
/// the new grammar.
fn validate_data_decls(
    new_grammar: &mut ResolvedGrammar,
    parsed_grammar: &ParsedGrammar,
    rule_sigs: &SignatureMap,
) -> fault::DynamicRes<()> {
    parsed_grammar
        .data_decls
        .iter()
        .try_for_each(|parsed_data_decl| {
            let new_data_decl = (parsed_data_decl, rule_sigs, parsed_grammar).try_into()?;

            if let Some(_overwritten) = new_grammar
                .data_decls
                .insert(parsed_data_decl.name.clone(), new_data_decl)
            {
                return Err(fault::DynamicErr::DuplicateDeclaration {
                    decl_name: parsed_data_decl.name.to_string(),
                });
            }

            Ok(())
        })?;

    Ok(())
}

pub type RuleSig = Vec<DataName>;
pub type SignatureMap = HashMap<RuleName, RuleSig>;

/// Returns a map of `RuleName`s and their corresponding signatures. It reads
/// the signatures of all `syntax::RuleDecl`s in the `ParsedGrammar` and for
/// each one, it does the lookup to verify that it's declared parameter types
/// are unambiguous and refer to actual `DataDecl`s.
fn validated_rule_signatures(parsed_grammar: &ParsedGrammar) -> fault::DynamicRes<SignatureMap> {
    parsed_grammar
        .rule_decls
        .iter()
        .map(|rule_decl| {
            let parameter_types = rule_decl
                .signature
                .parameter_types
                .iter()
                .map(|abbr_param_type| {
                    parsed_grammar
                        .data_decl_from_abbr_data_name(abbr_param_type)
                        .map(|data_decl| data_decl.name.clone())
                })
                .collect::<fault::DynamicRes<_>>()?;

            Ok((rule_decl.signature.name.clone(), parameter_types))
        })
        .collect()
}

pub struct DataDecl {
    pub variants: HashMap<DataVariant, Vec<SententialForm>>,
}

impl TryFrom<(&syntax::DataDecl, &SignatureMap, &ParsedGrammar)> for DataDecl {
    type Error = fault::DynamicErr;

    fn try_from(
        (parsed_data_decl, rule_sigs, parsed_grammar): (
            &syntax::DataDecl,
            &SignatureMap,
            &ParsedGrammar,
        ),
    ) -> Result<Self, Self::Error> {
        let variants = parsed_data_decl
            .variants
            .iter()
            .map(|(variant, stringifications)| {
                let new_stringifications = stringifications
                    .iter()
                    .map(|stringification| (stringification, rule_sigs, parsed_grammar).try_into())
                    .collect::<fault::DynamicRes<_>>()?;

                Ok((variant.clone(), new_stringifications))
            })
            .collect::<fault::DynamicRes<_>>()?;

        Ok(DataDecl { variants })
    }
}

pub struct SententialForm(Vec<Token>);

impl TryFrom<(&syntax::SententialForm, &SignatureMap, &ParsedGrammar)> for SententialForm {
    type Error = fault::DynamicErr;

    fn try_from(
        (parsed_sentential_form, rule_sigs, parsed_grammar): (
            &syntax::SententialForm,
            &SignatureMap,
            &ParsedGrammar,
        ),
    ) -> Result<Self, Self::Error> {
        let tokens = parsed_sentential_form
            .iter()
            .map(|parsed_token| (parsed_token, parsed_grammar, rule_sigs).try_into())
            .collect::<fault::DynamicRes<_>>()?;

        Ok(SententialForm(tokens))
    }
}

#[derive(Clone)]
pub enum Token {
    RuleRef(RuleRef),
    StrLit(IStr),
    DataVariable(DataVariable),
    DataVariant(DataVariant),
    Plus,
}

impl TryFrom<(&syntax::Token, &ParsedGrammar, &SignatureMap)> for Token {
    type Error = fault::DynamicErr;

    fn try_from(
        (parsed_token, parsed_grammar, rule_sigs): (&syntax::Token, &ParsedGrammar, &SignatureMap),
    ) -> fault::DynamicRes<Self> {
        match parsed_token {
            syntax::Token::Plus => Ok(Token::Plus),
            syntax::Token::StrLit(s) => Ok(Self::StrLit(s.clone())),
            syntax::Token::RuleRef(rule_ref) => (rule_ref, parsed_grammar, rule_sigs)
                .try_into()
                .map(Self::RuleRef),
            syntax::Token::DataVariable(variable) => (variable, parsed_grammar)
                .try_into()
                .map(Self::DataVariable),
            syntax::Token::DataVariant(ref abbr_variant) => (abbr_variant, parsed_grammar)
                .try_into()
                .map(Self::DataVariant),
        }
    }
}

impl TryFrom<(&Abbr<DataVariant>, &ParsedGrammar)> for DataVariant {
    type Error = fault::DynamicErr;

    fn try_from(
        (parsed_variant, parsed_grammar): (&Abbr<DataVariant>, &ParsedGrammar),
    ) -> fault::DynamicRes<Self> {
        let (_decl, variant) = parsed_grammar.data_decl_from_abbr_variant(parsed_variant)?;
        Ok(variant.clone())
    }
}

#[derive(Clone)]
pub struct RuleRef {
    pub rule: RuleName,
    pub args: Vec<Argument>,
}

impl TryFrom<(&syntax::RuleRef, &ParsedGrammar, &SignatureMap)> for RuleRef {
    type Error = fault::DynamicErr;
    fn try_from(
        (parsed_rule_ref, parsed_grammar, rule_sigs): (
            &syntax::RuleRef,
            &ParsedGrammar,
            &SignatureMap,
        ),
    ) -> fault::DynamicRes<Self> {
        // First lookup the unabbreviated rule name. Make sure it refers to an
        // actual `RuleDecl`.
        let (_rule_decl, rule_name) =
            parsed_grammar.rule_decl_from_abbr_rule_name(&parsed_rule_ref.rule)?;

        // Lookup the canonicalized `RuleName` in the `SignatureMap` to get the
        // expected parameter types.
        let sig = rule_sigs
            .get(rule_name)
            .expect("this RuleName should have been valid!");

        // Next, verify that the `Argument`s being passed in are of the correct
        // arity.
        if sig.len() != parsed_rule_ref.args.len() {
            return Err(fault::DynamicErr::WrongArityRuleReference {
                rule_name: rule_name.to_string(),
                call_site: parsed_rule_ref.to_string(),
                expected_len: sig.len(),
            });
        }

        // Finally, check that each argument conforms to its corresponding
        // parameter's type.
        let args = parsed_rule_ref
            .args
            .iter()
            .zip(sig)
            .map(|(arg, param_type)| (arg, param_type, parsed_rule_ref, parsed_grammar).try_into())
            .collect::<fault::DynamicRes<_>>()?;

        Ok(RuleRef {
            rule: rule_name.clone(),
            args,
        })
    }
}

#[derive(Clone)]
pub enum Argument {
    Variant(DataVariant),
    Variable(DataVariable),
}

impl
    TryFrom<(
        &syntax::Argument,
        &DataName,
        &syntax::RuleRef,
        &ParsedGrammar,
    )> for Argument
{
    type Error = fault::DynamicErr;
    fn try_from(
        (arg, param_type, parsed_rule_ref, parsed_grammar): (
            &syntax::Argument,
            &DataName,
            &syntax::RuleRef,
            &ParsedGrammar,
        ),
    ) -> Result<Self, Self::Error> {
        let expected_data_decl = parsed_grammar
            .data_decls
            .iter()
            .find(|decl| &decl.name == param_type)
            .expect("a corresponding DataDecl with this name exists");

        match arg {
            // If the argument is a `DataVariant`, we just need make sure it refers to a
            // variant defined in the `DataDecl`.
            syntax::Argument::Variant(abbr_variant) => {
                match expected_data_decl.lookup_variant(abbr_variant) {
                    Ok(data_variant) => Ok(Argument::Variant(data_variant.clone())),
                    Err(fault::DynamicErr::UnboundSymbol { symbol }) => {
                        Err(fault::DynamicErr::UnknownDataVariantInRuleRef {
                            abbr_variant: symbol,
                            data_type_name: param_type.to_string(),
                            rule_ref: parsed_rule_ref.rule.to_string(),
                        })
                    }
                    Err(e) => Err(e),
                }
            }

            // If the argument is a `DataVariable`, then we need to verify that it unambiguously
            // uses an appropriate abbreviation of the correct `DataDecl`'s name. And that's
            // basically it.
            syntax::Argument::Variable(abbr_variable) => {
                let data_decl = parsed_grammar.data_decl_from_abbr_variable(abbr_variable)?;
                let syntax::DataVariable(_, number) = abbr_variable;

                Ok(Argument::Variable(DataVariable {
                    data_name: data_decl.name.clone(),
                    number: number.clone(),
                }))
            }
        }
    }
}

#[derive(Clone)]
pub struct DataVariable {
    pub data_name: DataName,
    pub number: IStr,
}

impl TryFrom<(&syntax::DataVariable, &ParsedGrammar)> for DataVariable {
    type Error = fault::DynamicErr;

    fn try_from(
        (parsed_variable, parsed_grammar): (&syntax::DataVariable, &ParsedGrammar),
    ) -> fault::DynamicRes<Self> {
        let data_decl = parsed_grammar.data_decl_from_abbr_variable(parsed_variable)?;
        let syntax::DataVariable(_name, number) = parsed_variable;
        Ok(DataVariable {
            data_name: data_decl.name.clone(),
            number: number.clone(),
        })
    }
}

pub struct RuleDecl {
    pub cases: Vec<Case>,
}

impl RuleDecl {
    pub fn arity(&self) -> usize {
        let case = self
            .cases
            .first()
            .expect("Invariant Violated: some rule doesn't have any cases!");

        case.arity()
    }
}

impl
    TryFrom<(
        &syntax::RuleDecl,
        &RuleName,
        &RuleSig,
        &SignatureMap,
        &ParsedGrammar,
    )> for RuleDecl
{
    type Error = fault::DynamicErr;

    fn try_from(
        (parsed_rule_decl, rule_name, rule_sig, rule_sigs, parsed_grammar): (
            &syntax::RuleDecl,
            &RuleName,
            &RuleSig,
            &SignatureMap,
            &ParsedGrammar,
        ),
    ) -> Result<Self, Self::Error> {
        // To translate a `RuleDecl`, just translate each of its `Case`s.
        let cases = parsed_rule_decl
            .cases
            .iter()
            .map(|case| (case, rule_name, rule_sig, rule_sigs, parsed_grammar).try_into())
            .collect::<fault::DynamicRes<_>>()?;

        Ok(RuleDecl { cases })
    }
}

pub struct Case {
    pub requirements: Vec<Pattern>,
    pub alternatives: Vec<SententialForm>,
}

impl Case {
    pub fn arity(&self) -> usize {
        self.requirements.len()
    }

    pub fn covers<'iter>(
        &self,
        args: impl Iterator<Item = &'iter &'iter &'iter DataVariant>,
    ) -> bool {
        for (patt, arg) in self.requirements.iter().zip(args.into_iter()) {
            match patt {
                Pattern::Star | Pattern::Variable(_) => continue,
                Pattern::Variant(variant) if variant == **arg => continue,
                Pattern::Variant(_) => return false,
            }
        }

        true
    }
}

impl
    TryFrom<(
        &syntax::Case,
        &RuleName,
        &RuleSig,
        &SignatureMap,
        &ParsedGrammar,
    )> for Case
{
    type Error = fault::DynamicErr;

    fn try_from(
        (parsed_case, rule_name, rule_sig, rule_sigs, parsed_grammar): (
            &syntax::Case,
            &RuleName,
            &RuleSig,
            &SignatureMap,
            &ParsedGrammar,
        ),
    ) -> Result<Self, Self::Error> {
        // Check that the requirements have correct arity.
        if parsed_case.guard.requirements.len() != rule_sig.len() {
            return Err(fault::DynamicErr::WrongArityCaseGuard {
                rule_name: rule_name.to_string(),
                guard: parsed_case.guard.to_string(),
                expected_len: rule_sig.len(),
            });
        }

        // Check that the requirements have correct types.
        let requirements = parsed_case
            .guard
            .requirements
            .iter()
            .zip(rule_sig)
            .map(|(parsed_pattern, expected_type)| {
                (parsed_pattern, rule_name, expected_type, parsed_grammar).try_into()
            })
            .collect::<fault::DynamicRes<_>>()?;

        // Translate the sentential-form alternatives.
        let alternatives = parsed_case
            .alternatives
            .iter()
            .map(|alternative| (alternative, rule_sigs, parsed_grammar).try_into())
            .collect::<fault::DynamicRes<_>>()?;

        Ok(Case {
            requirements,
            alternatives,
        })
    }
}

pub enum Pattern {
    Star,
    Variant(DataVariant),
    Variable(DataVariable),
}

impl TryFrom<(&syntax::Pattern, &RuleName, &DataName, &ParsedGrammar)> for Pattern {
    type Error = fault::DynamicErr;

    fn try_from(
        (parsed_pattern, rule_name, expected_type, parsed_grammar): (
            &syntax::Pattern,
            &RuleName,
            &DataName,
            &ParsedGrammar,
        ),
    ) -> Result<Self, Self::Error> {
        match parsed_pattern {
            syntax::Pattern::Star => Ok(Pattern::Star),

            // If it's a `Abbr<DataVariant>`, ensure it is a member of the
            // expected `DataDecl`'s variants list.
            syntax::Pattern::Variant(abbr_variant) => {
                let expected_data_decl = parsed_grammar
                    .data_decls
                    .iter()
                    .find(|data_decl| &data_decl.name == expected_type)
                    .expect(&format!(
                        "Expected {} to be a valid `DataName`!",
                        expected_type,
                    ));

                let resolved_variant = expected_data_decl.lookup_variant(abbr_variant)?;

                Ok(Pattern::Variant(resolved_variant.clone()))
            }

            // If its a `syntax::DataVariable`, then we need to:
            //     1. translate it into a `resolve::DataVariable`, and
            //     2. verify that it's it refers to the correct `DataDecl`.
            syntax::Pattern::Variable(abbr_variable) => {
                let resolved_variable: DataVariable = (abbr_variable, parsed_grammar).try_into()?;

                if &resolved_variable.data_name != expected_type {
                    return Err(fault::DynamicErr::PatternMatchTypeError {
                        rule_name: rule_name.to_string(),
                        expected_type: expected_type.to_string(),
                        variable_type: resolved_variable.data_name.to_string(),
                        pattern_variable: abbr_variable.to_string(),
                    });
                }

                Ok(Pattern::Variable(resolved_variable))
            }
        }
    }
}
