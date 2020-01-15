use crate::syntax::{Grammar, Rule, Token};
use im::{vector, Vector};
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag},
    character::complete::{char, multispace0, multispace1, one_of},
    combinator::map,
    eof,
    multi::{many1, separated_list, separated_nonempty_list},
    re_find,
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};
use std::{cell::RefCell, rc::Rc};
use string_interner::{StringInterner, Sym};

type Syms = Rc<RefCell<StringInterner<Sym>>>;

fn literal<'i>(input: &'i str, syms: Syms) -> IResult<&'i str, Sym> {
    // See: https://python-reference.readthedocs.io/en/latest/docs/str/escapes.html
    //
    // \a           ASCII bell
    // \b           ASCII backspace
    // \f           ASCII formfeed
    // \n           ASCII linefeed
    // \N{name}     character named NAME in the Unicode database
    // \r           ASCII carriage return
    // \t           ASCII horizontal tab
    // \uxxxx       character with 16-bit hex value XXXX
    // \Uxxxxxxxx   character with 32-bit hex value XXXXXXXX
    // \v           ASCII vertical tab
    // \ooo         character with octal value OOO
    // \hxx         Character with hex value XX
    let double_quoted_str_escape = r#"\"abfnNrtuUvx01234567"#;
    let double_quoted = delimited(
        char('"'),
        escaped(is_not(r#""\"#), '\\', one_of(double_quoted_str_escape)),
        char('"'),
    );
    let (rest, content) = double_quoted(input)?;
    let sym = syms.borrow_mut().get_or_intern(content);
    Ok((rest, sym))
}

#[test]
fn test_literal() {
    let syms = Rc::new(RefCell::new(StringInterner::default()));
    let (_, actual) = literal(r#""inside of string""#, syms.clone()).expect("parse should succeed");
    let actual = syms.borrow().resolve(actual).unwrap().to_string();
    let expected = "inside of string".to_string();
    assert_eq!(actual, expected);
}

fn variable<'i>(input: &'i str, syms: Syms) -> IResult<&'i str, Sym> {
    let (rest, name) = re_find!(input, r"^([a-zA-Z_][a-zA-Z0-9_]*)")?;
    let sym = syms.borrow_mut().get_or_intern(name);
    Ok((rest, sym))
}

#[test]
fn test_variable() {
    let syms = Rc::new(RefCell::new(StringInterner::default()));
    let (_, actual) = variable(r#"some_symbol"#, syms.clone()).expect("parse should succeed");
    let actual = syms.borrow().resolve(actual).unwrap().to_string();
    let expected = "some_symbol".to_string();
    assert_eq!(actual, expected);
}

fn token<'i>(input: &'i str, syms: Syms) -> IResult<&'i str, Token> {
    let literal = |input| literal(input, syms.clone());
    let variable = |input| variable(input, syms.clone());
    alt((map(literal, Token::Lit), map(variable, Token::Var)))(input)
}

/// Accepts one or more space (' ') characters.
fn many1_space(input: &str) -> IResult<&str, ()> {
    let (rest, _) = many1(char(' '))(input)?;
    Ok((rest, ()))
}

type Sentence = Vector<Token>;

fn sentence(input: &str, syms: Syms) -> IResult<&str, Sentence> {
    let token = |input| token(input, syms.clone());
    let (rest, vec) = separated_list(many1_space, token)(input)?;
    let vector = vec.into_iter().collect();
    Ok((rest, vector))
}

#[test]
fn test_sentence() {
    let syms = Rc::new(RefCell::new(StringInterner::default()));
    let resolve = |s| syms.borrow().resolve(s).unwrap().to_string();
    let (_, actual) = sentence(r#"x y "lit"   z"#, syms.clone()).expect("parse should succeed");
    let actual = actual
        .into_iter()
        .map(|tok| match tok {
            Token::Var(s) => Token::Var(resolve(s)),
            Token::Lit(s) => Token::Lit(resolve(s)),
        })
        .collect::<Vec<_>>();
    let expected = vec![
        Token::Var("x".into()),
        Token::Var("y".into()),
        Token::Lit("lit".into()),
        Token::Var("z".into()),
    ];
    assert_eq!(actual, expected);
}

/// A rule is specified like this:
/// ```ignore
/// english_sentence -> subject " eats " object "."
///                   | subject " hits " object " with a bat."
///                   ;
/// ```
/// TODO: make this syntax nicer.
fn rule(input: &str, syms: Syms) -> IResult<&str, Vec<Rule>> {
    let variable = |input| variable(input, syms.clone());
    let arrow = tuple((multispace0, tag("->"), multispace0));
    let pipe = delimited(multispace0, char('|'), multispace0);
    let sentence = |input| sentence(input, syms.clone());

    let (rest, (head, _, bodies, _, _)) = tuple((
        variable,
        arrow,
        separated_list(pipe, sentence),
        multispace0,
        char(';'),
    ))(input)?;

    let rules = bodies
        .into_iter()
        .map(|body| Rule {
            head: head.clone(),
            pred: vector![],
            body,
        })
        .collect();
    Ok((rest, rules))
}

pub fn parse_source(src: &str, syms: Syms) -> IResult<&str, Grammar> {
    let input = src;
    let rule = |input| rule(input, syms.clone());
    let rules = separated_nonempty_list(multispace1, rule);
    let src_file = preceded(
        multispace0,
        terminated(terminated(rules, multispace0), |input: &str| eof!(input,)),
    );
    let (rest, rules_unflattened) = src_file(input)?;
    let rules = rules_unflattened.into_iter().flatten().collect();
    Ok((rest, Grammar { rules }))
}
