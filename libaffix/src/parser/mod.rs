mod space;
pub mod syntax;
pub mod typo;

use im::Vector;
use std::convert::TryFrom;
// use macro_rules_attribute::macro_rules_attribute;
use internship::IStr;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while, take_while1, take_while_m_n},
    character::complete::{anychar, char, one_of},
    combinator::{all_consuming, cut, map, opt, recognize, verify},
    error::context,
    multi::{many0, many1, separated_nonempty_list},
    sequence::{delimited, preceded, tuple},
    IResult,
};

use crate::fault::StaticErr;
use syntax::{
    Abbr, Argument, Case, DataDecl, DataName, DataVariable, DataVariant, Grammar, Guard, Pattern,
    RuleDecl, RuleName, RuleRef, RuleSig, SententialForm, Token,
};
use typo::{Report, SourcedTypo, Typo};

type Res<'input, Output> = IResult<&'input str, Output, Report<&'input str>>;

/// Requires one or more non-uppercase alphanumeric characters or the
/// underscore. Examples: `foo`, `blue42`, `bar_baz_qux`, `1st`, `_`
fn lower_ident(i: &str) -> Res<IStr> {
    let valid_char = |c: char| (c.is_alphanumeric() && !c.is_uppercase()) || c == '_';
    let (i, name) = context(
        "a lowercase identifier",
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
        "an uppercase identifier",
        recognize(preceded(
            take_while_m_n(1, 1, char::is_uppercase),
            take_while(char::is_alphabetic),
        )),
    )(i)?;
    Ok((i, IStr::new(name)))
}

/// Parses an `upper_ident` then an optional numeric suffix.
/// Examples: `Person`, `Person1`, `Person321`, `Person0`
fn variable(i: &str) -> Res<DataVariable> {
    let (i, name) = upper_ident(i)?;
    let (i, number) = take_while(char::is_numeric)(i)?;
    Ok((i, DataVariable(Abbr::new(name), number.into())))
}

#[test]
fn parse_variable() {
    let (_rest, actual) = variable("Gender123").unwrap();
    assert_eq!(
        actual,
        DataVariable(Abbr::new("Gender".into()), "123".into())
    );

    let (_rest, actual) = variable("Gender①②③").unwrap();
    assert_eq!(
        actual,
        DataVariable(Abbr::new("Gender".into()), "①②③".into())
    );

    let (_rest, actual) = variable("Gender").unwrap();
    assert_eq!(actual, DataVariable(Abbr::new("Gender".into()), "".into()));
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
    context(
        "a single data variant",
        tuple((
            space::allowed::after(map(lower_ident, DataVariant)),
            map(
                opt(delimited(
                    char('('),
                    space::allowed::around(sentential_form_alternatives),
                    char(')'),
                )),
                |opt_alternatives| opt_alternatives.unwrap_or_else(Vec::new),
            ),
        )),
    )(i)
}

/// Parses:
/// ```ignore
/// data Foo = variant_1 | variant_2 | variant_n
/// ```
fn data_decl(i: &str) -> Res<DataDecl> {
    let (i, (name, variants)) = preceded(
        space::required::after(tag("data")),
        context(
            "a data variant definition",
            cut(tuple((
                space::allowed::after(upper_ident),
                preceded(
                    char('='),
                    cut(separated_nonempty_list(
                        space::allowed::before(char('|')),
                        space::allowed::before(data_variant_decl),
                    )),
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
    use std::collections::BTreeMap;
    use std::iter::FromIterator;

    let src = "data Number = singular | plural";
    let (rest, parsed) = data_decl(src).expect("successful parse");
    assert_eq!(rest, "");
    let decl = DataDecl {
        name: DataName(IStr::new("Number")),
        variants: BTreeMap::from_iter(vec![
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
        map(variable, Argument::Variable),
        map(lower_ident, |ident| {
            Argument::Variant(Abbr::new(DataVariant(ident)))
        }),
        failure_case(char('*'), |_| {
            Typo::Custom(
                "USE OF '*' IN ARGUMENT POSITION",
                format!(
                    "I'm not supposed to allow the '*' character here. Please \
                    spell out a unique variable name if you'd like to pass a \
                    random value here."
                ),
            )
        }),
    ))(i)
}

/// Parsed a reference to a rule like: `start.G1.present_tense` or `story`.
/// Note: spaces are **not** allowed adjacent to the dots (`.`).
fn rule_ref(i: &str) -> Res<RuleRef> {
    let (i, name) = lower_ident(i)?;
    let (i, vars) = many0(preceded(char('.'), argument))(i)?;
    let reference = RuleRef {
        rule: Abbr::new(RuleName(name)),
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
    let unexpected_keyword = failure_case(alt((tag("rule"), tag("data"))), |kw| {
        Typo::Custom(
            "UNEXPECTED KEYWORD",
            format!(
                "I can't let you use an identifier named '{}' here. It's a \
                reserved keyword.",
                kw,
            ),
        )
    });

    let bad_data_interpolation = failure_case(anychar, |ch| {
        Typo::Custom(
            "BAD VALUE FOR DATA INTERPOLATION",
            format!(
                "I was expecting either the lowercase name of a data variant \
                value, or the uppercase name of a data variant type. Instead I \
                got {:?}.",
                ch,
            ),
        )
    });

    let (i, vec) = context(
        "a sentential form",
        separated_nonempty_list(
            space::required::here,
            alt((
                map(quoted, Token::StrLit),
                map(char('+'), |_| Token::Plus),
                preceded(
                    char('@'),
                    context(
                        "a data interpolation",
                        cut(alt((
                            map(lower_ident, |sym| Token::DataVariant(DataVariant(sym))),
                            map(variable, Token::DataVariable),
                            unexpected_keyword,
                            bad_data_interpolation,
                        ))),
                    ),
                ),
                context("a rule reference", map(rule_ref, Token::RuleRef)),
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
        "a rule signature",
        tuple((
            lower_ident,
            many0(preceded(
                char('.'),
                map(upper_ident, |ident| Abbr::new(DataName(ident))),
            )),
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
        "a pattern",
        alt((
            map(char('*'), |_| Pattern::Star),
            map(lower_ident, |ident| {
                Pattern::Variant(Abbr::new(DataVariant(ident)))
            }),
            map(variable, Pattern::Variable),
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
            "an arrow guard rule case",
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
                "a nested rule case",
                cut(space::allowed::before(many0(space::allowed::after(
                    guarded_rule_case(curr_guard.clone()),
                )))),
            ),
            context("a closing brace", cut(char('}'))),
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
            failure_case(anychar, |_| {
                Typo::Custom(
                    "MALFORMED RULE CASE BODY",
                    format!(
                        "I was expecting either '->' or '{{' here because I just \
                        saw a pattern guard."
                    ),
                )
            }),
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
                    "a sentential form rule body",
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
            "a rule definition",
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
    F: Fn(Parsed) -> Typo,
{
    use nom::Err::{Error, Failure, Incomplete};
    move |i: &'i str| match parser(i) {
        Ok((_i_after_parse, parsed)) => {
            let report = Report::from(SourcedTypo {
                fragment: i,
                typo: err_constructor(parsed),
            });
            Err(Failure(report))
        }
        Err(Failure(e)) => Err(Failure(e)),
        Err(Error(e)) => Err(Error(e)),
        Err(Incomplete(need)) => Err(Incomplete(need)),
    }
}

/// The top-level parsing function of this module. Attempts to parse a
/// [`Grammar`] from an input `&str`.
/// ```rust
/// use nom::error::VerboseError;
/// # use libaffix::parser::parse;
/// let res = parse("data Foo = bar | baz");
/// assert!(res.is_ok());
/// ```
pub fn parse(i: &str) -> Res<Grammar> {
    enum Decl {
        Data(DataDecl),
        Rule(RuleDecl),
    }

    let malformed_keyword = failure_case(recognize(rule_sig), |txt| {
        Typo::Custom(
            "MALFORMED KEYWORD",
            format!(
                "I expected either the 'data' or 'rule' keyword here, but I \
                got '{}'.",
                txt,
            ),
        )
    });

    let misplaced_data_decl = failure_case(recognize(upper_ident), |_| {
        Typo::Custom(
            "MISPLACED DATA DEFINITION",
            format!(
                "Is this the beginning of a data declaration? If so, it needs \
                to begin with the 'data' keyword."
            ),
        )
    });

    let extra_closing_brace = failure_case(char('}'), |_| {
        Typo::Custom(
            "EXTRA CLOSING BRACE",
            format!(
                "I think you have an extra closing brace ('}}') here. No big \
                deal, it happens!"
            ),
        )
    });

    let extraneous_char = failure_case(one_of("!@#$%^&*()[]{|<>?/~`\"\\:;="), |ch| {
        Typo::Custom(
            "EXTRANEOUS CHARACTER AFTER TOP-LEVEL DEFINITION",
            format!(
                "I got some weird character {:?} here. I'm not sure what's \
                going on. Do you?",
                ch,
            ),
        )
    });

    let (i, decls) = all_consuming(space::allowed::after(context(
        "the full grammar",
        many0(context(
            "a top-level definition",
            space::allowed::before(alt((
                map(data_decl, Decl::Data),
                map(rule_decl, Decl::Rule),
                malformed_keyword,
                misplaced_data_decl,
                extra_closing_brace,
                extraneous_char,
            ))),
        )),
    )))(i)?;

    let mut grammar = Grammar::default();

    for decl in decls {
        match decl {
            Decl::Data(decl) => grammar.data_decls.push(decl),
            Decl::Rule(decl) => grammar.rule_decls.push(decl),
        }
    }

    Ok((i, grammar))
}

/// You must keep the src string alive until after the function terminates so
/// that errors that reference either of them can be propagated up.
impl<'src> TryFrom<&'src str> for Grammar {
    type Error = StaticErr<'src>;
    fn try_from(src: &'src str) -> Result<Self, Self::Error> {
        use nom::Err::{Error, Failure, Incomplete};

        let (_unparsed_src, grammar) = parse(src).map_err(|e| match e {
            Failure(report) | Error(report) => StaticErr::from(report.summarize(src)),
            Incomplete(_) => unimplemented!(),
        })?;

        Ok(grammar)
    }
}

#[test]
fn parse_full_grammar() {
    use im::vector;
    use internship::IStr;
    use std::convert::TryInto;

    let src = r#"
data Number = singular | plural
data Person = 1st | 2nd | 3rd -- This is a comment.

rule start = "I" want.singular.1st "YOU!"

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

    let actual: Grammar = src.try_into().unwrap();

    let make_guard = |v: &[&str]| Guard {
        requirements: v
            .into_iter()
            .map(|s| Pattern::Variant(Abbr::new(DataVariant(IStr::new(s)))))
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
        rule_decls: vec![
            RuleDecl {
                signature: RuleSig {
                    name: RuleName(IStr::new("start")),
                    parameter_types: vec![],
                },
                cases: vec![Case {
                    guard: make_guard(&[]),
                    alternatives: vec![vector![
                        Token::StrLit(IStr::new("I")),
                        Token::RuleRef(RuleRef {
                            rule: Abbr::new(RuleName(IStr::new("want"))),
                            vars: vec![
                                Argument::Variant(Abbr::new(DataVariant(IStr::new("singular")))),
                                Argument::Variant(Abbr::new(DataVariant(IStr::new("1st"))))
                            ]
                        }),
                        Token::StrLit(IStr::new("YOU!"))
                    ]],
                }],
            },
            RuleDecl {
                signature: RuleSig {
                    name: RuleName(IStr::new("want")),
                    parameter_types: vec![
                        Abbr::new(DataName(IStr::new("Number"))),
                        Abbr::new(DataName(IStr::new("Person"))),
                    ],
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
            },
        ],
    };

    assert_eq!(actual, expected);
}
