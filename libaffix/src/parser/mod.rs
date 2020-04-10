mod space;
pub mod syntax;
pub mod typo;

use im::Vector;
// use macro_rules_attribute::macro_rules_attribute;
use internship::IStr;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while, take_while1, take_while_m_n},
    character::complete::char,
    combinator::{all_consuming, cut, map, opt, recognize, verify},
    error::context,
    multi::{many0, many1, separated_nonempty_list},
    sequence::{delimited, preceded, tuple},
    IResult,
};

use syntax::{
    Argument, Case, DataDecl, DataName, DataVariable, DataVariant, Grammar, Guard, Pattern,
    RuleDecl, RuleName, RuleRef, RuleSig, SententialForm, Token,
};

type Res<'input, Output> = IResult<&'input str, Output, typo::Report<&'input str>>;

/// Requires one or more non-uppercase alphanumeric characters or the
/// underscore. Examples: `foo`, `blue42`, `bar_baz_qux`, `1st`, `_`
fn lower_ident(i: &str) -> Res<IStr> {
    let valid_char = |c: char| (c.is_alphanumeric() && !c.is_uppercase()) || c == '_';
    let (i, name) = context(
        "lowercase identifier",
        verify(take_while1(valid_char), |txt: &str| {
            txt != "data" && txt != "rule"
        }),
    )(i)?;
    Ok((i, IStr::new(name)))
}

/// Requires exactly one uppercase character, then zero or more alphabetic
/// characters. Examples: `Person`, `Q`, `AbstractSingletonBean`
fn upper_ident(i: &str) -> Res<IStr> {
    let (i, name) = context(
        "uppercase identifier",
        recognize(preceded(
            take_while_m_n(1, 1, char::is_uppercase),
            take_while(char::is_alphabetic),
        )),
    )(i)?;
    Ok((i, IStr::new(name)))
}

fn quoted(i: &str) -> Res<IStr> {
    let (i, content) = delimited(char('"'), is_not("\""), char('"'))(i)?;
    Ok((i, IStr::new(content)))
}

/// Parses:
/// EITHER
/// ```ignore
/// identifier
/// ```
/// OR
/// ```ignore
/// identifier ( sentential_form_1 | sentential_form_2 | ... )
/// ```
fn data_variant_decl(i: &str) -> Res<(DataVariant, Vec<SententialForm>)> {
    tuple((
        map(space::allowed::after(lower_ident), DataVariant),
        map(
            opt(delimited(
                space::allowed::after(char('(')),
                space::allowed::after(sentential_form_alternatives),
                char(')'),
            )),
            |opt_alternatives| opt_alternatives.unwrap_or_else(Vec::new),
        ),
    ))(i)
}

/// Parses:
/// ```ignore
/// data Foo = variant_1 | variant_2 | variant_n
/// ```
fn data_decl(i: &str) -> Res<DataDecl> {
    let (i, (name, variants)) = preceded(
        space::required::after(tag("data")),
        context(
            "data variant definition",
            cut(tuple((
                space::allowed::after(upper_ident),
                preceded(
                    space::allowed::after(char('=')),
                    separated_nonempty_list(space::allowed::around(char('|')), data_variant_decl),
                ),
            ))),
        ),
    )(i)?;

    let decl = DataDecl {
        name: DataName(name),
        variants: variants.into_iter().collect(),
    };

    Ok((i, decl))
}

#[test]
fn parse_data_decl() {
    use internship::IStr;
    use std::collections::HashMap;
    use std::iter::FromIterator;

    let src = "data Number = singular | plural";
    let (rest, parsed) = data_decl(src).expect("successful parse");
    assert_eq!(rest, "");
    let decl = DataDecl {
        name: DataName(IStr::new("Number")),
        variants: HashMap::from_iter(vec![
            (DataVariant(IStr::new("singular")), vec![]),
            (DataVariant(IStr::new("plural")), vec![]),
        ]),
    };
    assert_eq!(parsed, decl);
}

/// Arguments passed to a rule ref (call) like: `start.G1.N2` or
/// `story.short.to_the_point`.
fn argument(i: &str) -> Res<Argument> {
    alt((
        map(upper_ident, |ident| Argument::Variable(DataVariable(ident))),
        map(lower_ident, |ident| Argument::Variant(DataVariant(ident))),
    ))(i)
}

/// Parsed a reference to a rule like: `start.G1.present_tense` or `story`.
/// Note: spaces are **not** allowed adjacent to the dots (`.`).
fn rule_ref(i: &str) -> Res<RuleRef> {
    let (i, name) = lower_ident(i)?;
    let (i, vars) = many0(preceded(char('.'), argument))(i)?;
    let reference = RuleRef {
        rule: RuleName(name),
        vars,
    };
    Ok((i, reference))
}

/// A sequence of string literals, plus-signs, rule references (calls), or
/// variable references.
/// Note: Spaces are allowed between items, but not required. This means that
/// each individual item that could appear in a sentential form **must** have
/// be able to be smashed up against any other (without whitespace) and be
/// parseable.
fn sentential_form(i: &str) -> Res<SententialForm> {
    let (i, vec) = context(
        "sentential form",
        separated_nonempty_list(
            space::allowed::here,
            alt((
                map(quoted, Token::StrLit),
                map(char('+'), |_| Token::Plus),
                preceded(
                    char('@'),
                    alt((
                        map(lower_ident, |sym| Token::DataVariant(DataVariant(sym))),
                        map(upper_ident, |sym| Token::DataVariable(DataVariable(sym))),
                    )),
                ),
                context("rule reference", map(rule_ref, Token::RuleRef)),
            )),
        ),
    )(i)?;
    Ok((i, Vector::from(vec)))
}

/// Parses the (type) signature a rule like: `start.Gender.Number` or `story`.
/// Appears at the start of a rule definition.
/// Note: spaces are **not** allowed adjacent to the dots (`.`).
fn rule_sig(i: &str) -> Res<RuleSig> {
    let (i, (name, vars)) = context(
        "rule signature",
        tuple((
            lower_ident,
            many0(preceded(char('.'), map(upper_ident, DataName))),
        )),
    )(i)?;

    let sig = RuleSig {
        name: RuleName(name),
        parameter_types: vars,
    };

    Ok((i, sig))
}

/// Any case-analysis pattern that can appear at the front of a case branch.
/// Parses `ident` or `*`.
fn pattern(i: &str) -> Res<Pattern> {
    context(
        "pattern",
        alt((
            map(char('*'), |_| Pattern::Star),
            map(lower_ident, |ident| Pattern::Variant(DataVariant(ident))),
        )),
    )(i)
}

/// Parses:
/// ```ignore
/// .ident_1.*.ident_2.*.*.ident_n
/// ```
/// Note: spaces are **not** allowed adjacent to the dots (`.`).
fn guard(i: &str) -> Res<Guard> {
    let (i, requirements) = many1(preceded(char('.'), pattern))(i)?;
    let guard = Guard {
        requirements: requirements.into(),
    };
    Ok((i, guard))
}

/// Parses:
/// ```ignore
/// sentential_form_1 | sentential_form_2 | sentential_form_n
/// ```
fn sentential_form_alternatives(i: &str) -> Res<Vec<SententialForm>> {
    separated_nonempty_list(space::allowed::around(char('|')), sentential_form)(i)
}

/// Parses a set of sentential form alternatives in the context of a `Guard` and
/// creates a `Case`.
/// Example:
/// ```ignore
/// sentential_form_1 | sentential_form_2 | sentential_form_n
/// ```
fn guarded_sentential_form_alternatives<'i>(guard: Guard) -> impl Fn(&'i str) -> Res<'i, Case> {
    move |i: &'i str| {
        map(sentential_form_alternatives, |alternatives| Case {
            guard: guard.clone(),
            alternatives,
        })(i)
    }
}

/// Parses:
/// ```ignore
/// .foo.bar.*.baz -> sentential_form_1 | sentential_form_2 | sentential_form_n
/// ```
fn arrow_guard_rule_case<'i>(curr_guard: Guard) -> impl Fn(&'i str) -> Res<'i, Case> {
    move |i: &'i str| {
        let (i, _) = space::allowed::after(tag("->"))(i)?;
        context(
            "arrow guard rule case",
            cut(guarded_sentential_form_alternatives(curr_guard.clone())),
        )(i)
    }
}

/// Parses:
/// ```ignore
/// .foo.bar.baz { rule_case_1 rule_case_2 rule_case_n }
/// ```
fn nested_guard_rule_case<'i>(curr_guard: Guard) -> impl Fn(&'i str) -> Res<'i, Vec<Case>> {
    move |i: &'i str| {
        let (i, cases) = delimited(
            char('{'),
            context(
                "nested rule case",
                cut(space::allowed::before(many0(space::allowed::after(
                    guarded_rule_case(curr_guard.clone()),
                )))),
            ),
            context("closing brace", cut(char('}'))),
        )(i)?;
        let cases = cases.into_iter().flatten().collect();
        Ok((i, cases))
    }
}

/// Parses either:
/// ```ignore
/// .foo.bar.*.baz -> sentential_form_1 | sentential_form_2 | sentential_form_n
/// ```
/// or:
/// ```ignore
/// .foo.bar.*.baz { rule_case_1 rule_case_2 rule_case_n }
/// ```
fn guarded_rule_case<'i>(curr_guard: Guard) -> impl Fn(&'i str) -> Res<'i, Vec<Case>> {
    move |i: &'i str| {
        let mut curr_guard = curr_guard.clone();
        let (i, more_guard) = space::allowed::after(guard)(i)?;
        curr_guard.append(&more_guard);
        alt((
            map(arrow_guard_rule_case(curr_guard.clone()), |case| vec![case]),
            nested_guard_rule_case(curr_guard.clone()),
        ))(i)
    }
}

/// Can either be:
/// - a list of sentential-form alternatives like:
///     - `"Once upon a time..." rest_of_story + "." | "The end."`
/// - or a guarded rule case like:
///     - `.foo.bar -> some_sentential_form`, or
///     - `.foo { nested_rule_cases }`
fn top_level_rule_cases<'i>(guard: Guard) -> impl Fn(&'i str) -> Res<'i, Vec<Case>> {
    move |i: &'i str| {
        let flatten = |v: Vec<_>| v.into_iter().flatten().collect();
        let (i, cases) = alt((
            map(
                separated_nonempty_list(space::required::here, guarded_rule_case(guard.clone())),
                flatten,
            ),
            map(
                context(
                    "sentential form rule body",
                    guarded_sentential_form_alternatives(guard.clone()),
                ),
                |case| vec![case],
            ),
        ))(i)?;
        Ok((i, cases))
    }
}

/// Parses a rule which looks like:
/// ```ignore
/// rule foo.Bar.Baz = <rule body here>
/// ```
fn rule_decl(i: &str) -> Res<RuleDecl> {
    let (i, (signature, cases)) = preceded(
        space::required::after(tag("rule")),
        context(
            "rule definition",
            cut(tuple((
                rule_sig,
                preceded(
                    space::allowed::around(char('=')),
                    top_level_rule_cases(Guard::default()),
                ),
            ))),
        ),
    )(i)?;
    let decl = RuleDecl { signature, cases };
    Ok((i, decl))
}

/// If the given subparser parses successfully, this parser will raise a
/// `nom::Err::Failure`. The `err_constructor` argument is a `Fn` that must
/// produce a `typo::Typo` given a `Parsed` value from `parser`.
fn failure_case<'i, Free, Parsed, P, F>(
    parser: P,
    err_constructor: F,
) -> impl Fn(&'i str) -> Res<'i, Free>
where
    P: Fn(&'i str) -> Res<'i, Parsed>,
    F: Fn(Parsed) -> typo::Typo,
{
    use nom::Err::{Error, Failure, Incomplete};
    use typo::Report;
    move |i: &'i str| match parser(i) {
        Ok((_, x)) => Err(Failure(Report::from((i, err_constructor(x))))),
        Err(Failure(e)) => Err(Failure(e)),
        Err(Error(e)) => Err(Error(e)),
        Err(Incomplete(need)) => Err(Incomplete(need)),
    }
}

/// The top-level parsing function of this module. Attempts to parse a
/// [`Grammar`] from an input `&str`. Note: you'll need to provide an error type
/// via the turbo-fish operator in order to constrain the generic error type
/// parameter. I recommend using [`nom::error::VerboseError<&str>`] for
/// debugging.
/// ```rust
/// use nom::error::VerboseError;
/// # use libaffix::parser::parse;
/// let res = parse::<VerboseError<&str>>("data Foo = bar | baz");
/// assert!(res.is_ok());
/// ```
pub fn parse(i: &str) -> Res<Grammar> {
    enum Decl {
        Data(DataDecl),
        Rule(RuleDecl),
    }

    let malformed_keyword = failure_case(recognize(rule_sig), |txt| {
        typo::Typo::Custom(format!(
            "I expected either the 'data' or 'rule' keyword here, but I got '{}'.",
            txt,
        ))
    });

    let misplaced_data_decl = failure_case(recognize(upper_ident), |_| {
        typo::Typo::Custom(format!("Is this the beginning of a data declaration? If so, it needs to begin with the 'data' keyword."))
    });

    let (i, decls) = context(
        "full grammar",
        all_consuming(space::allowed::before(many0(context(
            "top-level definition",
            space::allowed::after(alt((
                map(data_decl, Decl::Data),
                map(rule_decl, Decl::Rule),
                malformed_keyword,
                misplaced_data_decl,
            ))),
        )))),
    )(i)?;

    let mut grammar = Grammar::default();

    for decl in decls {
        match decl {
            Decl::Data(decl) => grammar.data_decls.push(decl),
            Decl::Rule(decl) => grammar.rule_decls.push(decl),
        }
    }

    Ok((i, grammar))
}

#[test]
fn parse_decl() {
    use im::vector;
    use internship::IStr;

    let src = r#"
data Number = singular | plural
data Person = 1st | 2nd | 3rd -- This is a comment.

rule want.Number.Person =
    .singular {
        .1st -> "veux"
        .2nd -> "veux"
        .3rd -> "veut"
    }
    .plural {
        .1st -> "voulons"
        .2nd -> "voulez"
        .3rd -> "voulent"
    }
    --comment at veeerry end"#;

    let (remainder, actual) = match parse(src) {
        Ok(pair) => pair,
        Err(nom::Err::Failure(e)) | Err(nom::Err::Error(e)) => {
            panic!("Parse Failure:\n{}", e.report(src));
        }
        _ => unimplemented!(),
    };
    assert!(remainder.is_empty());

    let make_guard = |v: &[&str]| Guard {
        requirements: v
            .into_iter()
            .map(|s| Pattern::Variant(DataVariant(IStr::new(s))))
            .collect(),
    };
    let sentence = |s: &str| vector![Token::StrLit(IStr::new(s))];

    let expected = Grammar {
        data_decls: vec![
            DataDecl {
                name: DataName(IStr::new("Number")),
                variants: vec![IStr::new("singular"), IStr::new("plural")]
                    .into_iter()
                    .map(DataVariant)
                    .map(|x| (x, vec![]))
                    .collect(),
            },
            DataDecl {
                name: DataName(IStr::new("Person")),
                variants: vec![IStr::new("1st"), IStr::new("2nd"), IStr::new("3rd")]
                    .into_iter()
                    .map(DataVariant)
                    .map(|x| (x, vec![]))
                    .collect(),
            },
        ],
        rule_decls: vec![RuleDecl {
            signature: RuleSig {
                name: RuleName(IStr::new("want")),
                parameter_types: vec![DataName(IStr::new("Number")), DataName(IStr::new("Person"))],
            },
            cases: vec![
                Case {
                    guard: make_guard(&["singular", "1st"]),
                    alternatives: vec![sentence("veux")],
                },
                Case {
                    guard: make_guard(&["singular", "2nd"]),
                    alternatives: vec![sentence("veux")],
                },
                Case {
                    guard: make_guard(&["singular", "3rd"]),
                    alternatives: vec![sentence("veut")],
                },
                Case {
                    guard: make_guard(&["plural", "1st"]),
                    alternatives: vec![sentence("voulons")],
                },
                Case {
                    guard: make_guard(&["plural", "2nd"]),
                    alternatives: vec![sentence("voulez")],
                },
                Case {
                    guard: make_guard(&["plural", "3rd"]),
                    alternatives: vec![sentence("voulent")],
                },
            ],
        }],
    };

    assert_eq!(actual, expected);
}
