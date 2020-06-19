use crate::parser::{
    syntax::{DataVariant, RuleDecl, RuleName},
    typo,
};
use thiserror::Error;

pub type StaticRes<'src, T = ()> = std::result::Result<T, StaticErr<'src>>;
pub type DynamicRes<T = ()> = std::result::Result<T, DynamicErr>;

#[non_exhaustive]
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

#[non_exhaustive]
#[derive(Error, Debug, Serialize)]
pub enum DynamicErr {
    #[error(
        "No case of rule `{rule_name}` matches the current arguments: \
        {arguments}"
    )]
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

    #[error("I'm not sure what the symbol `{symbol}` refers to. Did you mistype?")]
    UnboundSymbol { symbol: String },

    #[error(
        "Ambiguous variable name `{symbol}`! Could refer to either \
        `{possibility1}` or `{possibility2}`."
    )]
    AmbiguousSymbol {
        symbol: String,
        possibility1: String,
        possibility2: String,
    },

    #[error(
        "I don't know how to print the datavariant `@{symbol}` in a \
        user-friendly way!"
    )]
    NoDataVariantStringification { symbol: String },

    #[error(
        "Maximum iterations exceeded! This may indicate an unbounded recursive \
        rule, or just that sentence complexity has increased beyond what was \
        initially predicted. In the latter case, simply increase max trials \
        from {trials} and try again."
    )]
    MaxTrialsExceeded { trials: usize },

    #[error(
        "I got the wrong number of values sent to the rule `{rule_name}`! I \
        needed {expected_len} argument(s), but the call-site looks like this: \
        `{call_site}`."
    )]
    WrongArityRuleReference {
        rule_name: String,
        call_site: String,
        expected_len: usize,
    },

    #[error(
        "Hmm... According to its signature, the rule `{rule_name}` accepts \
        {expected_len} argument(s), but one of your case guards—the one that \
        looks like `{guard}`—expects a different number of arguments."
    )]
    WrongArityCaseGuard {
        rule_name: String,
        guard: String,
        expected_len: usize,
    },

    // TODO: rewrite this error message when code line-column can be provided!
    #[error(
        "It looks like you're trying to use a variable whose type is \
        `{variable_type}` in one of the case arms of the rule `{rule_name}`. \
        According the `{rule_name}`'s signature, only data variants of \
        `{expected_type}` will be matched against the pattern variable \
        `{pattern_variable}`. But you've named it in such a way that people \
        reading your grammar might think that variants of a `{variable_type}`
        will be bound to `{pattern_variable}`. Could you rename the variable \
        for me please?"
    )]
    PatternMatchTypeError {
        rule_name: String,
        expected_type: String,
        variable_type: String,
        pattern_variable: String,
    },

    #[error(
        "Hey, I'm not sure which data variant you're referring to here. You \
        tried calling `{rule_ref}` with the argument `{abbr_variant}`, but \
        that's not a variant that's defined in the `{data_type_name}` data \
        declaration. What gives?"
    )]
    UnknownDataVariantInRuleRef {
        abbr_variant: String,
        data_type_name: String,
        rule_ref: String,
    },

    #[error(
        "Oops! Looks like you have *two* declarations named `{decl_name}`! \
        Which one do you want to keep?"
    )]
    DuplicateDeclaration { decl_name: String },
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
            .map(|typ_name| typ_name.clone().abbreviation());
        let arg_names = arguments.iter().map(|evald_arg| evald_arg.0.as_str());
        let bindings = arg_names
            .zip(typ_names)
            .map(|(value, typ)| Binding {
                typ: typ.to_string(),
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
