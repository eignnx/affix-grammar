//! Walks a `parser::syntax::ParsedGrammar` and transforms it into a `checked::syntax::Grammar`. As
//! this is happening, static semantic errors may be thrown.

use crate::fault;
use crate::parser::syntax::{self, Abbr, DataName, DataVariant, ParsedGrammar, RuleName};
use im::Vector;
use internship::IStr;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};

pub struct Grammar {
    pub data_decls: HashMap<DataName, DataDecl>,
    pub rule_decls: HashMap<RuleName, RuleDecl>,
}

impl Default for Grammar {
    fn default() -> Self {
        Self {
            data_decls: Default::default(),
            rule_decls: Default::default(),
        }
    }
}

/// The main translation impl from `syntax::ParsedGrammar` to `semantics::Grammar`.
impl std::convert::TryFrom<ParsedGrammar> for Grammar {
    type Error = fault::DynamicErr;

    fn try_from(parsed_grammar: ParsedGrammar) -> fault::DynamicRes<Self> {
        let mut new_grammar = Grammar::default();

        // First we need to get the unambiguously-typed signatures of all the
        // rules.
        let rule_sigs = validated_rule_signatures(&parsed_grammar)?;

        validate_data_decls(&mut new_grammar, &parsed_grammar, &rule_sigs)?;

        Ok(new_grammar)
    }
}

/// Validates all the `syntax::DataDecl`s, and adds the validated versions to
/// the new grammar.
fn validate_data_decls(
    new_grammar: &mut Grammar,
    parsed_grammar: &ParsedGrammar,
    rule_sigs: &SignatureMap,
) -> fault::DynamicRes<()> {
    parsed_grammar.data_decls.iter().try_for_each(|data_decl| {
        let mut variants: HashMap<DataVariant, Vec<SententialForm>> = HashMap::new();
        for (variant, stringifications) in &data_decl.variants {
            let mut new_stringifications = Vec::new();
            for stringification in stringifications {
                let mut new_stringification = Vector::new();
                for parsed_token in stringification {
                    let token = (parsed_token, parsed_grammar, rule_sigs).try_into()?;
                    new_stringification.push_back(token);
                }
                new_stringifications.push(new_stringification);
            }
            variants.insert(variant.clone(), new_stringifications);
        }

        if let Some(_overwritten) = new_grammar
            .data_decls
            .insert(data_decl.name.clone(), DataDecl { variants })
        {
            return Err(fault::DynamicErr::DuplicateDeclaration {
                decl_name: data_decl.name.to_string(),
            });
        }

        Ok(())
    })?;

    Ok(())
}

type RuleSig = Vec<DataName>;
type SignatureMap = HashMap<RuleName, RuleSig>;

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

pub type SententialForm = Vector<Token>;

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
            let arguments = parsed_rule_ref
                .args
                .iter()
                .map(|arg| arg.to_string())
                .collect();
            return Err(fault::DynamicErr::WrongArityRuleReference {
                rule_name: rule_name.to_string(),
                arguments,
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
                    Err(None) => Err(fault::DynamicErr::UnknownDataVariantInRuleRef {
                        abbr_variant: abbr_variant.to_string(),
                        data_type_name: param_type.to_string(),
                        rule_ref: parsed_rule_ref.rule.to_string(),
                    }),
                    Err(Some((variant1, variant2))) => Err(fault::DynamicErr::AmbiguousSymbol {
                        symbol: abbr_variant.to_string(),
                        possibility1: variant1.to_string(),
                        possibility2: variant2.to_string(),
                    }),
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

pub struct Case {
    pub requirements: Vec<Pattern>,
    pub alternatives: Vec<SententialForm>,
}

pub enum Pattern {
    Star,
    Variant(DataVariant),
    Variable(DataVariable),
}
