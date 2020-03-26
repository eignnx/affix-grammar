use im::Vector;
use internship::IStr;
use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::{many0, many1, separated_nonempty_list},
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

pub mod lex;
use lex::{keyword, lexeme, lower_ident, quoted, upper_ident, Kw, Lex};

pub mod syntax;
use syntax::{
    Argument, Case, DataDecl, DataName, DataVariable, DataVariant, Grammar, Guard, Pattern,
    RuleDecl, RuleName, RuleRef, RuleSig, SententialForm, Token,
};

type Res<'a, T> = IResult<&'a [Lex], T>;

/// Parses:
/// EITHER
/// ```ignore
/// identifier
/// ```
/// OR
/// ```ignore
/// identifier ( sentential_form_1 | sentential_form_2 | ... )
/// ```
fn data_variant_decl(input: &[Lex]) -> Res<(DataVariant, Vec<SententialForm>)> {
    tuple((
        map(lower_ident, DataVariant),
        map(
            opt(delimited(
                lexeme(Lex::LParen),
                sentential_form_alternatives,
                lexeme(Lex::RParen),
            )),
            |opt_alternatives| opt_alternatives.unwrap_or_else(Vec::new),
        ),
    ))(input)
}

/// Parses:
/// ```ignore
/// data Foo = variant_1 | variant_2 | variant_n
/// ```
fn data_decl(input: &[Lex]) -> Res<DataDecl> {
    let (rest, _) = keyword(Kw::Data)(input)?;
    let (rest, name) = upper_ident(rest)?;
    let (rest, _) = lexeme(Lex::Equals)(rest)?;
    let (rest, variants) = separated_nonempty_list(lexeme(Lex::Pipe), data_variant_decl)(rest)?;
    let decl = DataDecl {
        name: DataName(name),
        variants: variants.into_iter().collect(),
    };
    Ok((rest, decl))
}

#[test]
fn parse_data_decl() {
    use internship::IStr;
    use lex::Lexer;
    use std::collections::HashMap;
    use std::iter::FromIterator;

    let src = "data Number = singular | plural";
    let mut lexer = Lexer::from(src);
    let mut buf = vec![];
    let input = lexer.to_slice(&mut buf).expect("successful tokenization");
    let (rest, parsed) = data_decl(input).expect("successful parse");
    assert_eq!(rest, &[Lex::Eof]);
    let decl = DataDecl {
        name: DataName(IStr::new("Number")),
        variants: HashMap::from_iter(vec![
            (DataVariant(IStr::new("singular")), vec![]),
            (DataVariant(IStr::new("plural")), vec![]),
        ]),
    };
    assert_eq!(parsed, decl);
}

fn argument(input: &[Lex]) -> Res<Argument> {
    alt((
        map(upper_ident, |i| Argument::Variable(DataVariable(i))),
        map(lower_ident, |i| Argument::Variant(DataVariant(i))),
    ))(input)
}

/// Parsed a reference to a rule like: `start.G1.N2` or `story`.
fn rule_ref(input: &[Lex]) -> Res<RuleRef> {
    let (rest, name) = lower_ident(input)?;
    let (rest, vars) = many0(preceded(lexeme(Lex::Dot), argument))(rest)?;
    let reference = RuleRef {
        rule: RuleName(name),
        vars,
    };
    Ok((rest, reference))
}

/// A sequence of string literals, plus-signs, rule references (calls), or
/// variable references.
fn sentential_form(input: &[Lex]) -> Res<SententialForm> {
    let (rest, vec) = many1(alt((
        map(quoted, Token::StrLit),
        map(lexeme(Lex::Plus), |_| Token::Plus),
        map(preceded(lexeme(Lex::At), lower_ident), |sym| {
            Token::DataVariant(DataVariant(sym))
        }),
        map(preceded(lexeme(Lex::At), upper_ident), |sym| {
            Token::DataVariable(DataVariable(sym))
        }),
        map(rule_ref, Token::RuleRef),
    )))(input)?;
    Ok((rest, Vector::from(vec)))
}

/// Parses the (type) signature a rule like: `start.Gender.Number` or `story`. Appears
/// at the start of a rule definition.
fn rule_sig(input: &[Lex]) -> Res<RuleSig> {
    let (rest, name) = lower_ident(input)?;
    let (rest, vars) = many0(preceded(lexeme(Lex::Dot), upper_ident))(rest)?;
    let sig = RuleSig {
        name: RuleName(name),
        parameter_types: vars.into_iter().map(DataName).collect(),
    };
    Ok((rest, sig))
}

fn pattern(input: &[Lex]) -> Res<Pattern> {
    alt((
        map(lower_ident, |i| Pattern::Variant(DataVariant(i))),
        map(lexeme(Lex::Star), |_| Pattern::Star),
    ))(input)
}

/// Parses:
/// ```ignore
/// .ident_1.ident_2.ident_n
/// ```
fn guard(input: &[Lex]) -> Res<Guard> {
    let dot = lexeme(Lex::Dot);
    let (rest, requirements) = many1(preceded(dot, pattern))(input)?;
    let guard = Guard {
        requirements: requirements.into(),
    };
    Ok((rest, guard))
}

/// Parses:
/// ```ignore
/// sentential_form_1 | sentential_form_2 | sentential_form_n
/// ```
fn sentential_form_alternatives(input: &[Lex]) -> Res<Vec<SententialForm>> {
    separated_nonempty_list(lexeme(Lex::Pipe), sentential_form)(input)
}

/// Parses a set of sentential form alternatives in the context of a `Guard` and
/// creates a `Case`.
fn guarded_sentential_form_alternatives(guard: Guard) -> impl Fn(&[Lex]) -> Res<Case> {
    move |input| {
        map(sentential_form_alternatives, |alternatives| Case {
            guard: guard.clone(),
            alternatives,
        })(input)
    }
}

/// Parses:
/// ```ignore
/// .foo.bar.baz -> sentential_form_1 | sentential_form_2 | sentential_form_n
/// ```
fn guard_arrow_rule_case(curr_guard: Guard) -> impl Fn(&[Lex]) -> Res<Case> {
    move |input| {
        let mut curr_guard = curr_guard.clone();
        let (rest, more_guard) = guard(input)?;
        curr_guard.append(&more_guard);
        let (rest, _) = lexeme(Lex::Arrow)(rest)?;
        guarded_sentential_form_alternatives(curr_guard)(rest)
    }
}

/// Parses:
/// ```ignore
/// .foo.bar.baz { rule_case_1 rule_case_2 rule_case_n }
/// ```
fn nested_guard_rule_case(curr_guard: Guard) -> impl Fn(&[Lex]) -> Res<Vec<Case>> {
    move |input| {
        let lbrace = lexeme(Lex::LBrace);
        let rbrace = lexeme(Lex::RBrace);
        let mut curr_guard = curr_guard.clone();
        let (rest, more_guard) = guard(input)?;
        curr_guard.append(&more_guard);
        let (rest, cases) = delimited(lbrace, many0(rule_cases(curr_guard)), rbrace)(rest)?;
        let cases = cases.into_iter().flatten().collect();
        Ok((rest, cases))
    }
}

fn guarded_rule_case(guard: Guard) -> impl Fn(&[Lex]) -> Res<Vec<Case>> {
    move |input| {
        alt((
            map(guard_arrow_rule_case(guard.clone()), |case| vec![case]),
            nested_guard_rule_case(guard.clone()),
        ))(input)
    }
}

/// Can either be:
/// - a list of sentential-form alternatives, or
/// - a guarded rule case like:
///     - `.foo.bar -> some_sentential_form`, or
///     - `.foo { nested_rule_cases }`
fn rule_cases(guard: Guard) -> impl Fn(&[Lex]) -> Res<Vec<Case>> {
    move |input| {
        let flatten = |v: Vec<_>| v.into_iter().flatten().collect();
        let (rest, cases) = alt((
            map(many1(guarded_rule_case(guard.clone())), flatten),
            map(
                guarded_sentential_form_alternatives(guard.clone()),
                |case| vec![case],
            ),
        ))(input)?;
        Ok((rest, cases))
    }
}

fn rule_decl(input: &[Lex]) -> Res<RuleDecl> {
    let (rest, _) = keyword(Kw::Rule)(input)?;
    let (rest, signature) = rule_sig(rest)?;
    let (rest, _) = lexeme(Lex::Equals)(rest)?;
    let (rest, cases) = rule_cases(Guard::default())(rest)?;
    let decl = RuleDecl { signature, cases };
    Ok((rest, decl))
}

pub fn parse_from_lex_stream(input: &[Lex]) -> Res<Grammar> {
    enum Decl {
        Data(DataDecl),
        Rule(RuleDecl),
    }

    let (rest, decls) = terminated(
        many0(alt((
            map(data_decl, Decl::Data),
            map(rule_decl, Decl::Rule),
        ))),
        lexeme(Lex::Eof),
    )(input)?;

    let mut grammar = Grammar::default();

    for decl in decls {
        match decl {
            Decl::Data(d) => grammar.data_decls.push(d),
            Decl::Rule(r) => grammar.rule_decls.push(r),
        }
    }

    Ok((rest, grammar))
}

#[test]
fn parse_decl() {
    use im::vector;
    use internship::IStr;
    use lex::Lexer;

    let src = r#"
    data Number = singular | plural
    data Person = 1st | 2nd | 3rd

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
    "#;
    let mut lexer = Lexer::from(src);
    let mut buf = vec![];
    lexer.to_slice(&mut buf).expect("tokenization to succeed");
    let (rest, actual) = parse_from_lex_stream(&buf).expect("parse to succeed");
    assert!(rest.is_empty());

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
