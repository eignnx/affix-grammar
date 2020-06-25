use std::fmt;
use thiserror::Error;

pub type DynamicRes<T = ()> = std::result::Result<T, DynamicErr>;
pub type SemanticRes<T = ()> = std::result::Result<T, SemanticErr>;

#[non_exhaustive]
#[derive(Error, Debug, Serialize)]
pub enum DynamicErr {
    #[error(
        "Maximum iterations exceeded! This may indicate an unbounded recursive \
        rule, or just that sentence complexity has increased beyond what was \
        initially predicted. In the latter case, simply increase max trials \
        from {trials} and try again."
    )]
    MaxTrialsExceeded { trials: usize },
}

impl From<DynamicErr> for wasm_bindgen::JsValue {
    fn from(err: DynamicErr) -> wasm_bindgen::JsValue {
        let msg = "Failure to translate StaticErr into JsValue!";
        serde_wasm_bindgen::to_value(&err).expect(msg)
    }
}

#[derive(Debug, Serialize)]
pub enum StringificationUseCase {
    Variable { parsed_name: String },
    Variant { parsed_name: String },
}

impl fmt::Display for StringificationUseCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Variable { parsed_name } => write!(f, "the variable `{}`", parsed_name),
            Self::Variant { parsed_name } => write!(f, "the variant `{}`", parsed_name),
        }
    }
}

#[non_exhaustive]
#[derive(Error, Debug, Serialize)]
pub enum SemanticErr {
    #[error(
        "The cases for the rule `{rule_name}` are not exhaustive! For \
        instance, none of your cases would handle a rule invocation  like \
        `{rule_name}.{arguments:?}`."
    )]
    InexhaustiveCaseAnalysis {
        rule_name: String,
        arguments: Vec<String>,
    },

    #[error(
        "Case #{unused_case_number} in the rule `{rule_name}` is unnecessary \
        because it can never be reached. Just FYI."
    )]
    UnreacheableRuleCase {
        rule_name: String,
        unused_case_number: usize,
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
        "I don't know how to turn `{data_name}`'s data variant `@{variant}` \
        into user-facing text! This needs to be specified because you're \
        trying to @-print {use_case}."
    )]
    NoDataVariantStringification {
        use_case: StringificationUseCase,
        data_name: String,
        variant: String,
    },

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

impl From<SemanticErr> for wasm_bindgen::JsValue {
    fn from(err: SemanticErr) -> wasm_bindgen::JsValue {
        let msg = "Failure to translate DynamicErr into JsValue!";
        serde_wasm_bindgen::to_value(&err).expect(msg)
    }
}
