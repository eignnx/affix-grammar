use crate::parser::{
    syntax::{DataVariant, RuleDecl, RuleName},
    typo,
};
use thiserror::Error;

pub type StaticRes<'src, T = ()> = std::result::Result<T, StaticErr<'src>>;
pub type DynamicRes<T = ()> = std::result::Result<T, DynamicErr>;

#[derive(Error, Debug, Serialize)]
pub enum StaticErr<'src> {
    #[error("Syntax Error:\n{0}")]
    SyntaxErr(typo::ErrorSummary<'src>),
}

impl<'src> From<typo::ErrorSummary<'src>> for StaticErr<'src> {
    fn from(summary: typo::ErrorSummary<'src>) -> Self {
        Self::SyntaxErr(summary)
    }
}

impl<'src> From<StaticErr<'src>> for wasm_bindgen::JsValue {
    fn from(err: StaticErr<'src>) -> wasm_bindgen::JsValue {
        let msg = "Failure to translate StaticErr into JsValue!";
        serde_wasm_bindgen::to_value(&err).expect(msg)
    }
}

#[derive(Error, Debug, Serialize)]
pub enum DynamicErr {
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

impl DynamicErr {
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

impl<'src> From<DynamicErr> for wasm_bindgen::JsValue {
    fn from(err: DynamicErr) -> wasm_bindgen::JsValue {
        let msg = "Failure to translate DynamicErr into JsValue!";
        serde_wasm_bindgen::to_value(&err).expect(msg)
    }
}
