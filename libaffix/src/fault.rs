use crate::parser::lex::Lex;
use crate::parser::syntax::{DataVariant, RuleDecl, RuleName};
use thiserror::Error;
use wasm_bindgen::JsValue;

pub type Result<'buf, T = ()> = std::result::Result<T, Fault<'buf>>;

#[derive(Error, Debug)]
pub enum Fault<'buf> {
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
    BadTokenization(nom::Err<(&'buf str, nom::error::ErrorKind)>),

    #[error("Parsing failed at {0:?}")]
    BadParse(nom::Err<(&'buf [Lex], nom::error::ErrorKind)>),
}

#[derive(Debug)]
struct Binding {
    typ: String,
    value: String,
}

#[derive(Debug)]
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

impl<'buf> Fault<'buf> {
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

impl<'src> From<Fault<'src>> for JsValue {
    fn from(fault: Fault) -> JsValue {
        match fault {
            // TODO: convert to serde-serialized Js objects instead of strings.
            fault => format!("FAULT: {}", fault).into(),
        }
    }
}
