use crate::gen::Syms;
use crate::syntax::{EvalStmt, Grammar, Rule, TestStmt, Token};
use im::{vector, Vector};
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag},
    character::complete::{char, multispace0, multispace1, one_of},
    combinator::{map, opt},
    eof,
    multi::{separated_list, separated_nonempty_list},
    re_find,
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use string_interner::Sym;

fn string_literal(syms: Syms) -> impl Fn(&str) -> IResult<&str, Sym> {
    move |input| {
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
}

#[test]
fn test_literal() {
    let syms = crate::gen::make_symbol_pool();
    let (_, actual) =
        string_literal(syms.clone())(r#""inside of string""#).expect("parse should succeed");
    let actual = syms.borrow().resolve(actual).unwrap().to_string();
    let expected = "inside of string".to_string();
    assert_eq!(actual, expected);
}

fn variable(syms: Syms) -> impl Fn(&str) -> IResult<&str, Sym> {
    move |input| {
        let (rest, name) = re_find!(input, r"^([a-zA-Z_][a-zA-Z0-9_]*)")?;
        let sym = syms.borrow_mut().get_or_intern(name);
        Ok((rest, sym))
    }
}

#[test]
fn test_variable() {
    let syms = crate::gen::make_symbol_pool();
    let (_, actual) = variable(syms.clone())("some_symbol").expect("parse should succeed");
    let actual = syms.borrow().resolve(actual).unwrap().to_string();
    let expected = "some_symbol".to_string();
    assert_eq!(actual, expected);
}

/// Accepts meta-statements like:
///     x: "y" z + "."
///     :x
///     !x
///     x
fn eval_stmt(syms: Syms) -> impl Fn(&str) -> IResult<&str, EvalStmt> {
    move |input| {
        let key = separated_pair(
            variable(syms.clone()),
            tuple((char(':'), multispace0)),
            token(syms.clone()),
        );
        let set = preceded(char(':'), variable(syms.clone()));
        let unset = preceded(char('!'), variable(syms.clone()));

        alt((
            map(key, |(key, value)| EvalStmt::Key(key, value)),
            map(set, |key| EvalStmt::Set(key)),
            map(unset, |key| EvalStmt::Unset(key)),
        ))(input)
    }
}

fn eval_stmts(syms: Syms) -> impl Fn(&str) -> IResult<&str, Vector<EvalStmt>> {
    move |input| {
        let comma_space = tuple((char(','), multispace0));
        let (rest, vec) = delimited(
            tuple((char('{'), multispace0)),
            separated_list(comma_space, eval_stmt(syms.clone())),
            tuple((multispace0, char('}'))),
        )(input)?;
        Ok((rest, Vector::from(vec)))
    }
}

/// Accepts meta-statements like:
///     x: "y"
///     x! "y"
///     :x
///     !x
fn test_stmt(syms: Syms) -> impl Fn(&str) -> IResult<&str, TestStmt> {
    move |input| {
        let key = separated_pair(
            variable(syms.clone()),
            tuple((char(':'), multispace0)),
            string_literal(syms.clone()),
        );
        let not_key = separated_pair(
            variable(syms.clone()),
            tuple((char('!'), multispace0)),
            string_literal(syms.clone()),
        );
        let set = preceded(char(':'), variable(syms.clone()));
        let unset = preceded(char('!'), variable(syms.clone()));

        alt((
            map(key, |(key, value)| TestStmt::Key(key, value)),
            map(not_key, |(key, value)| TestStmt::NotKey(key, value)),
            map(set, TestStmt::Set),
            map(unset, TestStmt::Unset),
        ))(input)
    }
}

fn test_stmts(syms: Syms) -> impl Fn(&str) -> IResult<&str, Vector<TestStmt>> {
    move |input| {
        let comma_space = tuple((char(','), multispace0));
        let (rest, vec) = delimited(
            tuple((char('{'), multispace0)),
            separated_list(comma_space, test_stmt(syms.clone())),
            tuple((multispace0, char('}'))),
        )(input)?;
        Ok((rest, Vector::from(vec)))
    }
}

/// Allows the user to write `+` which eats any implicit spaces beside it.
fn plus_literal(input: &str) -> IResult<&str, Token> {
    map(char('+'), |_| Token::Plus)(input)
}

fn token(syms: Syms) -> impl Fn(&str) -> IResult<&str, Token> {
    move |input| {
        alt((
            plus_literal,
            map(string_literal(syms.clone()), Token::Lit),
            map(variable(syms.clone()), Token::Var),
            map(eval_stmts(syms.clone()), Token::Meta),
        ))(input)
    }
}

type Sentence = Vector<Token>;

fn sentence(syms: Syms) -> impl Fn(&str) -> IResult<&str, Sentence> {
    move |input| {
        let (rest, vec) = separated_nonempty_list(multispace1, token(syms.clone()))(input)?;
        let vector = vec.into_iter().collect();
        Ok((rest, vector))
    }
}

#[test]
fn test_sentence() {
    let syms = crate::gen::make_symbol_pool();
    let resolve = |s| syms.borrow().resolve(s).unwrap().to_string();
    let (_, actual) = sentence(syms.clone())(r#"x y "lit"   z"#).expect("parse should succeed");
    let actual = actual
        .into_iter()
        .map(|tok| match tok {
            Token::Var(s) => Token::Var(resolve(s)),
            Token::Lit(s) => Token::Lit(resolve(s)),
            _ => unimplemented!(),
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
/// english_sentence -{x: "foo"}-> subject "eats" object + "."
///                  --> subject "hits" object "with a bat."
///                  ;
/// ```
fn rule(syms: Syms) -> impl Fn(&str) -> IResult<&str, Vec<Rule>> {
    move |input| {
        let pipe = delimited(multispace0, char('|'), multispace0);
        let optional_test = map(opt(test_stmts(syms.clone())), |x| x.unwrap_or(vector![]));
        let full_arrow = delimited(char('-'), optional_test, tag("->"));
        let semicolon = preceded(multispace0, char(';'));

        // guarded_rule_group ::= "-{optional_meta_guard}-> sentence | sentence | ... | sentence"
        let guarded_rule_group = tuple((
            terminated(full_arrow, multispace0),
            preceded(
                opt(&pipe),
                separated_nonempty_list(&pipe, sentence(syms.clone())),
            ),
        ));

        // rule_group ::= "variable guarded_rule_group guarded_rule_group ... guarded_rule_group ;"
        let rule_group = terminated(
            tuple((
                terminated(variable(syms.clone()), multispace0),
                separated_nonempty_list(multispace1, guarded_rule_group),
            )),
            semicolon,
        );

        let (rest, (head, parsed_rules)) = rule_group(input)?;

        let rules = parsed_rules
            .into_iter()
            .flat_map(|(guard_stmts, sentences)| {
                sentences.into_iter().map(move |sentence| Rule {
                    head: head.clone(),
                    test: guard_stmts.clone(),
                    body: sentence,
                })
            })
            .collect();

        Ok((rest, rules))
    }
}

pub fn parse_source(src: &str, syms: Syms) -> IResult<&str, Grammar> {
    let input = src;
    let rules = separated_nonempty_list(multispace1, rule(syms));
    let src_file = preceded(
        multispace0,
        terminated(terminated(rules, multispace0), |input: &str| eof!(input,)),
    );
    let (rest, rules_unflattened) = src_file(input)?;
    let rules = rules_unflattened.into_iter().flatten().collect();
    Ok((rest, Grammar { rules }))
}
