//! A module for parsing whitespace. Takes into account comments too.
#![allow(unused)]

use nom::{
    branch::alt,
    bytes::complete::{tag, take_till},
    character::complete::{multispace0, multispace1},
    combinator::recognize,
    error::ParseError,
    multi::many1,
    sequence::{delimited, preceded, terminated},
    IResult,
};

/// A comment starts with `--` and continues till the end of the line, or
/// end of input, whichever comes first. Note: this parser explicitly does
/// NOT consume the '\n' character at the end of lines.
fn comment<'i, E>(i: &'i str) -> IResult<&'i str, &'i str, E>
where
    E: ParseError<&'i str>,
{
    let (i, _) = tag("--")(i)?;
    let (i, content) = take_till(|ch| ch == '\n')(i)?;
    // Strip off the first space if it has one.
    if content.starts_with(' ') {
        Ok((i, &content[1..]))
    } else {
        Ok((i, content))
    }
}

pub mod allowed {
    use super::*;

    /// Whitespace is allowed here, but not required.
    pub fn here<'i, E>(i: &'i str) -> IResult<&'i str, &'i str, E>
    where
        E: ParseError<&'i str>,
    {
        alt((super::required::here, multispace0))(i)
    }

    /// Has potentially-empty whitespace before **and** after the captured parser.
    pub fn around<'i, T, E, P>(parser: P) -> impl Fn(&'i str) -> IResult<&'i str, T, E>
    where
        E: ParseError<&'i str>,
        P: Fn(&'i str) -> IResult<&'i str, T, E>,
    {
        move |i: &'i str| delimited(here, &parser, here)(i)
    }

    /// Has potentially-empty whitespace after the captured parser.
    pub fn after<'i, T, E, P>(parser: P) -> impl Fn(&'i str) -> IResult<&'i str, T, E>
    where
        E: ParseError<&'i str>,
        P: Fn(&'i str) -> IResult<&'i str, T, E>,
    {
        move |i: &'i str| terminated(&parser, here)(i)
    }

    /// Has potentially-empty whitespace before the captured parser.
    pub fn before<'i, T, E, P>(parser: P) -> impl Fn(&'i str) -> IResult<&'i str, T, E>
    where
        E: ParseError<&'i str>,
        P: Fn(&'i str) -> IResult<&'i str, T, E>,
    {
        move |i: &'i str| preceded(here, &parser)(i)
    }
}

pub mod required {
    use super::*;

    /// Whitespace is required here.
    pub fn here<'i, E>(i: &'i str) -> IResult<&'i str, &'i str, E>
    where
        E: ParseError<&'i str>,
    {
        recognize(many1(alt((multispace1, comment))))(i)
    }

    /// Has potentially-empty whitespace before **and** after the captured parser.
    pub fn around<'i, T, E, P>(parser: P) -> impl Fn(&'i str) -> IResult<&'i str, T, E>
    where
        E: ParseError<&'i str>,
        P: Fn(&'i str) -> IResult<&'i str, T, E>,
    {
        move |i: &'i str| delimited(here, &parser, here)(i)
    }

    /// Has potentially-empty whitespace after the captured parser.
    pub fn after<'i, T, E, P>(parser: P) -> impl Fn(&'i str) -> IResult<&'i str, T, E>
    where
        E: ParseError<&'i str>,
        P: Fn(&'i str) -> IResult<&'i str, T, E>,
    {
        move |i: &'i str| terminated(&parser, here)(i)
    }

    /// Has potentially-empty whitespace before the captured parser.
    pub fn before<'i, T, E, P>(parser: P) -> impl Fn(&'i str) -> IResult<&'i str, T, E>
    where
        E: ParseError<&'i str>,
        P: Fn(&'i str) -> IResult<&'i str, T, E>,
    {
        move |i: &'i str| preceded(here, &parser)(i)
    }
}
