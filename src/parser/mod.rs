use im::Vector;
use internship::IStr;
use nom::{
    branch::alt,
    combinator::map,
    multi::{many0, many1, separated_nonempty_list},
    sequence::{delimited, preceded, terminated},
    IResult,
};
use std::collections::HashSet;
pub mod lex;
use lex::{ident, keyword, lexeme, quoted, Kw, Lex};

/// The name of a rule.
#[derive(Debug, Clone, PartialEq)]
pub struct RuleName(pub IStr);

/// A variable that represents a data variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DataVariable(pub IStr);

/// The name of a data-type.
#[derive(Debug, Clone, PartialEq)]
pub struct DataName(pub IStr);

impl DataName {
    /// Performs an equality check with a `DataVariable` since a `DataVariable`
    /// can have a numeric suffix to distinguish it.
    pub fn matches_variable(&self, variable: &DataVariable) -> bool {
        let DataVariable(var_txt) = variable;
        let var_without_nums = var_txt.as_str().trim_end_matches(char::is_numeric);
        let DataName(name) = self;
        name.as_str().starts_with(var_without_nums)
    }
}

/// The name of a variant of a data-type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DataVariant(pub IStr);

/// The "call site" of a rule. Includes variables that should be referenced inside the call.
#[derive(Debug, Clone, PartialEq)]
pub struct RuleRef {
    pub rule: RuleName,
    pub vars: Vec<DataVariable>,
}

/// The type signature of a rule.
#[derive(Debug, Clone, PartialEq)]
pub struct RuleSig {
    pub name: RuleName,
    pub parameter_types: Vec<DataName>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    RuleRef(RuleRef),
    StrLit(IStr),
    Plus,
}

pub type SententialForm = Vector<Token>;

#[derive(Debug, PartialEq)]
pub struct DataDecl {
    pub name: DataName,
    pub variants: HashSet<DataVariant>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Guard {
    pub requirements: Vector<DataVariant>,
}

impl Guard {
    fn append(&mut self, other: &Self) {
        self.requirements.append(other.requirements.clone());
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct RuleBody {
    pub guard: Guard,
    pub sentential_form: SententialForm,
}

#[derive(Debug, PartialEq)]
pub struct RuleDecl {
    pub signature: RuleSig,
    pub bodies: Vec<RuleBody>,
}

#[derive(Debug, Default, PartialEq)]
pub struct Grammar {
    pub data_decls: Vec<DataDecl>,
    pub rule_decls: Vec<RuleDecl>,
}

type Res<'a, T> = IResult<&'a [Lex], T>;

fn data_decl(input: &[Lex]) -> Res<DataDecl> {
    let (rest, _) = keyword(Kw::Data)(input)?;
    let (rest, name) = ident(rest)?;
    let (rest, _) = lexeme(Lex::Equals)(rest)?;
    let (rest, variants) = separated_nonempty_list(lexeme(Lex::Pipe), ident)(rest)?;
    let decl = DataDecl {
        name: DataName(name),
        variants: variants.iter().map(|s| DataVariant(IStr::new(s))).collect(),
    };
    Ok((rest, decl))
}

#[test]
fn parse_data_decl() {
    use lex::Lexer;
    use std::iter::FromIterator;

    let src = "data Number = singular | plural";
    let mut lexer = Lexer::from(src);
    let mut buf = vec![];
    let input = lexer.to_slice(&mut buf).expect("successful tokenization");
    let (rest, parsed) = data_decl(input).expect("successful parse");
    assert_eq!(rest, &[Lex::Eof]);
    let decl = DataDecl {
        name: DataName(IStr::new("Number")),
        variants: HashSet::from_iter(vec![
            DataVariant(IStr::new("singular")),
            DataVariant(IStr::new("plural")),
        ]),
    };
    assert_eq!(parsed, decl);
}

/// Parsed a reference to a rule like: `start.G1.N2` or `story`.
fn rule_ref(input: &[Lex]) -> Res<RuleRef> {
    let (rest, name) = ident(input)?;
    let (rest, vars) = many0(preceded(lexeme(Lex::Dot), ident))(rest)?;
    let reference = RuleRef {
        rule: RuleName(name),
        vars: vars.into_iter().map(DataVariable).collect(),
    };
    Ok((rest, reference))
}

fn sentential_form(input: &[Lex]) -> Res<SententialForm> {
    let (rest, vec) = many1(alt((
        map(quoted, Token::StrLit),
        map(lexeme(Lex::Plus), |_| Token::Plus),
        map(rule_ref, Token::RuleRef),
    )))(input)?;
    Ok((rest, Vector::from(vec)))
}

/// Parsed a reference to a rule like: `start.G1.N2` or `story`.
fn rule_sig(input: &[Lex]) -> Res<RuleSig> {
    let (rest, name) = ident(input)?;
    let (rest, vars) = many0(preceded(lexeme(Lex::Dot), ident))(rest)?;
    let sig = RuleSig {
        name: RuleName(name),
        parameter_types: vars.into_iter().map(DataName).collect(),
    };
    Ok((rest, sig))
}

fn guard(input: &[Lex]) -> Res<Guard> {
    let dot = lexeme(Lex::Dot);
    let (rest, requirements) = many1(preceded(dot, ident))(input)?;
    let guard = Guard {
        requirements: requirements.into_iter().map(DataVariant).collect(),
    };
    Ok((rest, guard))
}

fn sentential_form_alternatives(guard: Guard) -> impl Fn(&[Lex]) -> Res<Vec<RuleBody>> {
    move |input| {
        let pipe = lexeme(Lex::Pipe);
        let (rest, alternatives) = separated_nonempty_list(pipe, sentential_form)(input)?;
        let bodies = alternatives
            .into_iter()
            .map(|form| RuleBody {
                guard: guard.clone(),
                sentential_form: form,
            })
            .collect();
        Ok((rest, bodies))
    }
}

/// Parses:
/// ```no_run
/// .foo.bar.baz -> sentential_form_1 | sentential_form_2 | sentential_form_n
/// ```
fn guard_arrow_rule_body(curr_guard: Guard) -> impl Fn(&[Lex]) -> Res<Vec<RuleBody>> {
    move |input| {
        let mut curr_guard = curr_guard.clone();
        let (rest, more_guard) = guard(input)?;
        curr_guard.append(&more_guard);
        let (rest, _) = lexeme(Lex::Arrow)(rest)?;
        sentential_form_alternatives(curr_guard)(rest)
    }
}

/// Parses:
/// ```no_run
/// .foo.bar.baz { rule_body_1 rule_body_2 rule_body_n }
/// ```
fn nested_guard_rule_body(curr_guard: Guard) -> impl Fn(&[Lex]) -> Res<Vec<RuleBody>> {
    move |input| {
        let lbrace = lexeme(Lex::LBrace);
        let rbrace = lexeme(Lex::RBrace);
        let mut curr_guard = curr_guard.clone();
        let (rest, more_guard) = guard(input)?;
        curr_guard.append(&more_guard);
        let (rest, bodies) = delimited(lbrace, many0(rule_bodies(curr_guard)), rbrace)(rest)?;
        let bodies = bodies.into_iter().flatten().collect();
        Ok((rest, bodies))
    }
}

fn guarded_rule_body(guard: Guard) -> impl Fn(&[Lex]) -> Res<Vec<RuleBody>> {
    move |input| {
        alt((
            guard_arrow_rule_body(guard.clone()),
            nested_guard_rule_body(guard.clone()),
        ))(input)
    }
}

fn rule_bodies(guard: Guard) -> impl Fn(&[Lex]) -> Res<Vec<RuleBody>> {
    move |input| {
        let flatten = |v: Vec<_>| v.into_iter().flatten().collect();
        let (rest, bodies) = alt((
            map(many1(guarded_rule_body(guard.clone())), flatten),
            sentential_form_alternatives(guard.clone()),
        ))(input)?;
        Ok((rest, bodies))
    }
}

fn rule_decl(input: &[Lex]) -> Res<RuleDecl> {
    let (rest, _) = keyword(Kw::Rule)(input)?;
    let (rest, signature) = rule_sig(rest)?;
    let (rest, _) = lexeme(Lex::Equals)(rest)?;
    let (rest, bodies) = rule_bodies(Guard::default())(rest)?;
    let decl = RuleDecl { signature, bodies };
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
    use lex::Lexer;

    let src = r#"
    data Number = singular | plural

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

    let make_guard = |v: &[&str]| Guard {
        requirements: v.into_iter().map(|s| DataVariant(IStr::new(s))).collect(),
    };
    let sentence = |s: &str| vector![Token::StrLit(IStr::new(s))];

    let expected = Grammar {
        data_decls: vec![DataDecl {
            name: DataName(IStr::new("Number")),
            variants: vec![IStr::new("singular"), IStr::new("plural")]
                .into_iter()
                .map(DataVariant)
                .collect(),
        }],
        rule_decls: vec![RuleDecl {
            signature: RuleSig {
                name: RuleName(IStr::new("want")),
                parameter_types: vec![DataName(IStr::new("Number")), DataName(IStr::new("Person"))],
            },
            bodies: vec![
                RuleBody {
                    guard: make_guard(&["singular", "1st"]),
                    sentential_form: sentence("veux"),
                },
                RuleBody {
                    guard: make_guard(&["singular", "2nd"]),
                    sentential_form: sentence("veux"),
                },
                RuleBody {
                    guard: make_guard(&["singular", "3rd"]),
                    sentential_form: sentence("veut"),
                },
                RuleBody {
                    guard: make_guard(&["plural", "1st"]),
                    sentential_form: sentence("voulons"),
                },
                RuleBody {
                    guard: make_guard(&["plural", "2nd"]),
                    sentential_form: sentence("voulez"),
                },
                RuleBody {
                    guard: make_guard(&["plural", "3rd"]),
                    sentential_form: sentence("voulent"),
                },
            ],
        }],
    };

    assert_eq!(actual, expected);
}
