use crate::parser::syntax::{DataVariant, RuleDecl, RuleName};
use thiserror::Error;
use wasm_bindgen::JsValue;

pub type Result<T = ()> = std::result::Result<T, Fault>;

#[derive(Debug, Serialize)]
pub struct TokenizationErr {
    description: String,
}

impl<'buf> From<nom::Err<(&'buf str, nom::error::ErrorKind)>> for TokenizationErr {
    fn from(nom_err: nom::Err<(&'buf str, nom::error::ErrorKind)>) -> Self {
        TokenizationErr {
            // TODO: actually use info in `nom_err`
            description: format!("Tokenization Error: {}", nom_err),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct ParseErr {
    description: String,
}

impl<'i> From<nom::Err<(&'i str, nom::error::ErrorKind)>> for ParseErr {
    fn from(nom_err: nom::Err<(&'i str, nom::error::ErrorKind)>) -> Self {
        ParseErr {
            // TODO: actually use info in `nom_err`
            description: format!("Parse Error: {}", nom_err),
        }
    }
}

#[derive(Error, Debug, Serialize)]
pub enum Fault {
    #[allow(dead_code)]
    #[error("No case of rule `{rule_name}` matches the current arguments: {arguments}")]
    InexhaustiveCaseAnalysis {
        rule_name: String,
        arguments: ArgMap,
    },

    #[allow(dead_code)]
    #[error("The identifier `{rule_name}` is not the name of a rule!")]
    UnboundRuleName {
        rule_name: String, // TODO: tell user where in src error occurred
                           // TODO: move this into semantic analysis phase, not runtime
    },

    #[error("Tokenization failed at {0:?}")]
    BadTokenization(TokenizationErr),

    #[error("Parsing failed at {0:?}")]
    BadParse(ParseErr),
}

#[derive(Debug, Serialize)]
struct Binding {
    typ: String,
    value: String,
}

#[derive(Debug, Serialize)]
pub struct ArgMap(Vec<Binding>);

impl std::fmt::Display for ArgMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        let ArgMap(bindings) = self;
        for Binding { typ, value } in bindings {
            write!(f, "{} = .{}, ", typ, value)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl<'buf> Fault {
    pub fn inexhaustive_case_analysis(
        rules: &[RuleDecl],
        rule_name: &RuleName,
        arguments: &Vec<DataVariant>,
    ) -> Self {
        let typ_names = rules
            .iter()
            .filter(|decl| &decl.signature.name == rule_name)
            .next()
            .expect("rule to exist")
            .signature
            .parameter_types
            .iter()
            .map(|typ_name| typ_name.0.as_str());
        let arg_names = arguments.iter().map(|evald_arg| evald_arg.0.as_str());
        let bindings = arg_names
            .zip(typ_names)
            .map(|(value, typ)| Binding {
                typ: typ.into(),
                value: value.into(),
            })
            .collect::<Vec<_>>();
        Self::InexhaustiveCaseAnalysis {
            rule_name: rule_name.0.as_str().into(),
            arguments: ArgMap(bindings),
        }
    }

    pub fn unbound_rule_name(rule_name: &str) -> Self {
        Self::UnboundRuleName {
            rule_name: rule_name.into(),
        }
    }
}

impl<'src> From<Fault> for JsValue {
    fn from(fault: Fault) -> JsValue {
        match fault {
            // TODO: convert to serde-serialized Js objects instead of strings.
            fault => format!("FAULT: {}", fault).into(),
        }
    }
}
