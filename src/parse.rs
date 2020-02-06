use crate::gen::Syms;
use crate::lex::{lexeme, quoted, word, Lex, Lexer};
use crate::syntax::{EvalStmt, Grammar, Rule, TestStmt, Token};
use im::{vector, Vector};
use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::{many1, separated_list, separated_nonempty_list},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::fmt;
use string_interner::Sym;

type Res<'input, T> = IResult<&'input [Lex], T>;

fn string_literal(input: &[Lex]) -> Res<Sym> {
    quoted(input)
}

fn variable(input: &[Lex]) -> Res<Sym> {
    word(input)
}

fn sentence(input: &[Lex]) -> Res<Vector<Token>> {
    let (rest, vec) = many1(token)(input)?;
    let vector = vec.into_iter().collect();
    Ok((rest, vector))
}

/// Accepts meta-statements like:
///     x: "y" z + "."
///     :x
///     !x
///     x
fn eval_stmt(input: &[Lex]) -> Res<EvalStmt> {
    let key = separated_pair(variable, lexeme(Lex::Colon), token);
    let set = preceded(lexeme(Lex::Colon), variable);
    let unset = preceded(lexeme(Lex::Bang), variable);

    alt((
        map(key, |(key, value)| EvalStmt::KeyValue(key, value)),
        map(set, |key| EvalStmt::FlagSet(key)),
        map(unset, |key| EvalStmt::FlagUnset(key)),
    ))(input)
}

fn eval_stmts(input: &[Lex]) -> Res<Vector<EvalStmt>> {
    let (rest, vec) = delimited(
        lexeme(Lex::LBrace),
        separated_list(lexeme(Lex::Comma), eval_stmt),
        lexeme(Lex::RBrace),
    )(input)?;
    Ok((rest, Vector::from(vec)))
}

/// Accepts meta-statements like:
///     x: "y"
///     x! "y"
///     :x
///     !x
fn test_stmt(input: &[Lex]) -> Res<TestStmt> {
    let key = separated_pair(variable, lexeme(Lex::Colon), string_literal);
    let not_key = separated_pair(variable, lexeme(Lex::Bang), string_literal);
    let set = preceded(lexeme(Lex::Colon), variable);
    let unset = preceded(lexeme(Lex::Bang), variable);

    alt((
        map(key, |(key, value)| TestStmt::KeyValue(key, value)),
        map(not_key, |(key, value)| TestStmt::NotKeyValue(key, value)),
        map(set, TestStmt::FlagSet),
        map(unset, TestStmt::FlagUnset),
    ))(input)
}

fn test_stmts(input: &[Lex]) -> Res<Vector<TestStmt>> {
    let (rest, vec) = delimited(
        lexeme(Lex::LBrace),
        separated_list(lexeme(Lex::Comma), test_stmt),
        lexeme(Lex::RBrace),
    )(input)?;
    Ok((rest, Vector::from(vec)))
}

/// Allows the user to write `+` which eats any implicit spaces beside it.
fn plus_literal(input: &[Lex]) -> Res<Token> {
    map(lexeme(Lex::Plus), |_| Token::Plus)(input)
}

/// Matches either `(t1 t2 ... tn)` or `(t1 t2 ... tn)[v1, v2, ... vn]`.
fn scoped_sentence(input: &[Lex]) -> Res<(Vector<Token>, Vec<Sym>)> {
    tuple((
        delimited(lexeme(Lex::LParen), sentence, lexeme(Lex::RParen)),
        map(
            opt(delimited(
                lexeme(Lex::LBrack),
                separated_list(lexeme(Lex::Comma), variable),
                lexeme(Lex::RBrack),
            )),
            |vars_opt| vars_opt.unwrap_or_default(),
        ),
    ))(input)
}

fn token(input: &[Lex]) -> Res<Token> {
    alt((
        plus_literal,
        map(string_literal, Token::Lit),
        map(eval_stmts, Token::Meta),
        map(scoped_sentence, |(toks, vars)| Token::Scoped(toks, vars)),
        map(variable, Token::Var),
    ))(input)
}

/// A rule is specified like this:
/// ```ignore
/// english_sentence -{x: "foo"}-> subject "eats" object + "."
///                  --> subject "hits" object "with a bat."
///                  ;
/// ```
fn rule(input: &[Lex]) -> Res<Vec<Rule>> {
    let pipe = lexeme(Lex::Pipe);
    let optional_test = map(opt(test_stmts), |x| x.unwrap_or(vector![]));
    let arrow = delimited(
        lexeme(Lex::ArrowStart),
        optional_test,
        lexeme(Lex::ArrowEnd),
    );
    let semicolon = lexeme(Lex::Semicolon);

    // guarded_rule_group ::= "-{optional_meta_guard}-> sentence | sentence | ... | sentence"
    let guarded_rule_group = tuple((
        arrow,
        preceded(opt(&pipe), separated_nonempty_list(&pipe, sentence)),
    ));

    // rule_group ::= "variable guarded_rule_group guarded_rule_group ... guarded_rule_group ;"
    let rule_group = terminated(tuple((variable, many1(guarded_rule_group))), semicolon);

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

pub fn parse_source(input: &[Lex]) -> Res<Grammar> {
    let rules = terminated(many1(rule), lexeme(Lex::Eof));
    let (rest, rules_unflattened) = rules(input)?;
    let rules = rules_unflattened.into_iter().flatten().collect();
    Ok((rest, Grammar { rules }))
}

#[derive(Debug)]
pub enum ParseErr<'input> {
    TokenizationErr(nom::Err<(&'input str, nom::error::ErrorKind)>),
    GrammaticalErr(nom::Err<(&'input [Lex], nom::error::ErrorKind)>),
}

impl<'input> fmt::Display for ParseErr<'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TokenizationErr(nom::Err::Error((rest, e))) => {
                writeln!(f, "Tokenization error: {:?}", e)?;
                writeln!(f, "Error just before: ```\n{}\n```", rest)?;
            }
            Self::TokenizationErr(nom::Err::Failure((rest, e))) => {
                writeln!(f, "Tokenization failure: {:?}", e)?;
                writeln!(f, "Failed just before: ```\n{}\n```", rest)?;
            }
            Self::GrammaticalErr(nom::Err::Error((rest, e))) => {
                writeln!(f, "Grammatical error: {:?}", e)?;
                writeln!(f, "Error just before: ```\n{:?}\n```", rest)?;
            }
            Self::GrammaticalErr(nom::Err::Failure((rest, e))) => {
                writeln!(f, "Grammatical failure: {:?}", e)?;
                writeln!(f, "Failed just before: ```\n{:?}\n```", rest)?;
            }
            _ => unimplemented!("Some other kind of error"),
        }
        Ok(())
    }
}

impl<'input> std::error::Error for ParseErr<'input> {}

pub fn parse_grammar<'input>(
    src_txt: &'input str,
    syms: Syms,
    buf: &'input mut Vec<Lex>,
) -> Result<Grammar, ParseErr<'input>> {
    let mut lexer = Lexer::new(src_txt, syms);
    let tokens = lexer.to_slice(buf).map_err(ParseErr::TokenizationErr)?;
    let (_rest, grammar) = parse_source(tokens).map_err(ParseErr::GrammaticalErr)?;
    Ok(grammar)
}
