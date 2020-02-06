use crate::gen::{make_symbol_pool, Syms};
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag},
    character::complete::{char, multispace1, one_of},
    combinator::map,
    eof, re_find,
    sequence::{delimited, preceded},
    IResult,
};
use string_interner::Sym;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Lex {
    Word(Sym),
    Quoted(Sym),
    ArrowEnd,
    ArrowStart,
    Pipe,
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBrack,
    RBrack,
    Colon,
    Bang,
    Comma,
    Semicolon,
    Plus,
    Eof,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum WsLex {
    Ws,
    Comment,
    Lexeme(Lex),
}

pub struct Lexer<'input> {
    input: &'input str,
    syms: Syms,
    done: bool,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str, syms: Syms) -> Self {
        Self {
            input,
            syms,
            done: false,
        }
    }

    pub fn to_slice<'buf>(
        &mut self,
        buf: &'buf mut Vec<Lex>,
    ) -> Result<&'buf [Lex], LexErr<'input>> {
        for lexeme in self {
            buf.push(lexeme?);
        }
        Ok(buf.as_slice())
    }
}

impl<'input> From<&'input str> for Lexer<'input> {
    fn from(input: &'input str) -> Self {
        Self {
            input,
            syms: make_symbol_pool(),
            done: false,
        }
    }
}

type LexErr<'a> = nom::Err<(&'a str, nom::error::ErrorKind)>;

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<Lex, LexErr<'input>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        let lex = loop {
            let (rest, lex) = match lexeme_parser(self.input, self.syms.clone()) {
                Ok(tup) => tup,
                Err(e) => return Some(Err(e)),
            };
            self.input = rest;
            match lex {
                WsLex::Ws | WsLex::Comment => continue,
                WsLex::Lexeme(lex) => {
                    if lex == Lex::Eof {
                        self.done = true;
                    }
                    break lex;
                }
            }
        };
        Some(Ok(lex))
    }
}

fn lexeme_parser(input: &str, syms: Syms) -> IResult<&str, WsLex> {
    alt((
        map(
            alt((
                map(string_literal(syms.clone()), Lex::Quoted),
                map(variable(syms.clone()), Lex::Word),
                map(tag("->"), |_| Lex::ArrowEnd),
                map(tag("-"), |_| Lex::ArrowStart),
                map(char('|'), |_| Lex::Pipe),
                map(char('{'), |_| Lex::LBrace),
                map(char('}'), |_| Lex::RBrace),
                map(char('('), |_| Lex::LParen),
                map(char(')'), |_| Lex::RParen),
                map(char('['), |_| Lex::LBrack),
                map(char(']'), |_| Lex::RBrack),
                map(char(':'), |_| Lex::Colon),
                map(char('!'), |_| Lex::Bang),
                map(char(','), |_| Lex::Comma),
                map(char(';'), |_| Lex::Semicolon),
                map(char('+'), |_| Lex::Plus),
                map(eof_, |_| Lex::Eof),
            )),
            WsLex::Lexeme,
        ),
        map(comment, |_| WsLex::Comment),
        map(multispace1, |_| WsLex::Ws),
    ))(input)
}

#[test]
fn lexer() {
    let lexer = Lexer::from(
        r##"
        start --> "Once" upon {a: "time"} | there was;
        # this is a comment
        there -{foo: bar, !baz}-> quux;
        "##,
    );
    for lex in lexer {
        println!("{:?}", lex.expect("successful tokenization"));
    }
}

fn eof_(input: &str) -> IResult<&str, &str> {
    eof!(input,)
}

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

fn variable(syms: Syms) -> impl Fn(&str) -> IResult<&str, Sym> {
    move |input| {
        let (rest, name) = re_find!(input, r"^([a-zA-Z_][a-zA-Z0-9_]*)")?;
        let sym = syms.borrow_mut().get_or_intern(name);
        Ok((rest, sym))
    }
}

fn take_until_line_end_or_eof(input: &str) -> IResult<&str, &str> {
    for (i, ch) in input.char_indices() {
        if ch == '\n' {
            let (parsed, rest) = input.split_at(i + 1);
            return Ok((rest, parsed));
        }
    }
    Ok(("", input))
}

fn comment(input: &str) -> IResult<&str, &str> {
    preceded(char('#'), take_until_line_end_or_eof)(input)
}

pub fn word(input: &[Lex]) -> IResult<&[Lex], Sym> {
    input
        .split_first()
        .and_then(|(first, rest)| match first {
            Lex::Word(sym) => Some((rest, *sym)),
            _ => None,
        })
        .ok_or(nom::Err::Error((
            input,
            nom::error::ErrorKind::AlphaNumeric,
        )))
}

pub fn quoted(input: &[Lex]) -> IResult<&[Lex], Sym> {
    input
        .split_first()
        .and_then(|(first, rest)| match first {
            Lex::Quoted(sym) => Some((rest, *sym)),
            _ => None,
        })
        .ok_or(nom::Err::Error((
            input,
            nom::error::ErrorKind::AlphaNumeric,
        )))
}

pub fn lexeme(target: Lex) -> impl Fn(&[Lex]) -> IResult<&[Lex], Lex> {
    move |input| {
        input
            .split_first()
            .filter(|(first, _)| *first == &target)
            .map(|(first, rest)| (rest, *first))
            .ok_or(nom::Err::Error((
                input,
                nom::error::ErrorKind::AlphaNumeric,
            )))
    }
}

#[test]
fn parsing_of_lexemes() {
    use nom::sequence::{separated_pair, terminated};
    let mut lexer = Lexer::from("start : thing;");
    let mut buf = vec![];
    let input = lexer.to_slice(&mut buf).expect("successful tokenization");

    let res = terminated(
        separated_pair(word, lexeme(Lex::Colon), word),
        lexeme(Lex::Semicolon),
    )(input);

    res.expect("successful parse");
}