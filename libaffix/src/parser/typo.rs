#[derive(Debug, Clone)]
pub enum Typo {
    Custom(String),
    Context(&'static str),
    Expected(Vec<char>),
    Nom(nom::error::ErrorKind),
}

#[derive(Debug, Clone, Default)]
pub struct Report<I> {
    typos: Vec<(I, Typo)>,
}

impl<I> From<(I, Typo)> for Report<I> {
    fn from(tuple: (I, Typo)) -> Self {
        Self { typos: vec![tuple] }
    }
}

impl<I> nom::error::ParseError<I> for Report<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        Self {
            typos: vec![(input, Typo::Nom(kind))],
        }
    }

    fn append(input: I, kind: nom::error::ErrorKind, mut other: Self) -> Self {
        other.typos.push((input, Typo::Nom(kind)));
        other
    }

    fn from_char(input: I, c: char) -> Self {
        Self {
            typos: vec![(input, Typo::Expected(vec![c]))],
        }
    }

    /// If both `self` and `other` have `Typo::Expected` variants as their most
    /// recent typo, combine them. Otherwise, merge `self` and `other` and
    /// return `self`.
    fn or(mut self, mut other: Self) -> Self {
        let (my_last, ur_last) = (self.typos.last_mut(), other.typos.last_mut());
        if let (Some((_, my)), Some((_, ur))) = (my_last, ur_last) {
            if let (Typo::Expected(my), Typo::Expected(ur)) = (my, ur) {
                my.append(ur);
                self
            } else {
                self.typos.append(&mut other.typos);
                self
            }
        } else {
            self.typos.append(&mut other.typos);
            self
        }
    }

    fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
        other.typos.push((input, Typo::Context(ctx)));
        other
    }
}

use nom::Offset;
use std::fmt::Write;

impl<'i> Report<&'i str> {
    pub fn report(&self, input: &'i str) -> String {
        let mut result = String::new();

        for (i, (substring, kind)) in self.typos.iter().enumerate() {
            let offset = input.offset(substring);

            if input.is_empty() {
                match kind {
                    Typo::Custom(msg) => write!(
                        &mut result,
                        "Syntax Error #{}: {msg}",
                        i + 1,
                        msg = msg,
                    ),
                    Typo::Expected(chars) => write!(
                        &mut result,
                        "Syntax Error #{}: I expected one of {:?}, but got empty input.\n\n",
                        i + 1,
                        chars,
                    ),
                    Typo::Context(s) => write!(
                        &mut result,
                        "Syntax Error #{}: I was trying to parse a {}, but got empty input.\n\n",
                        i + 1,
                        s,
                    ),
                    Typo::Nom(e) => write!(
                        &mut result,
                        "Syntax Error #{}: I was trying to parse a {:?}, but got empty input.\n\n",
                        i + 1,
                        e,
                    ),
                }
            } else {
                let prefix = &input.as_bytes()[..offset];

                // Count the number of newlines in the first `offset` bytes of input
                let line_number = prefix.iter().filter(|&&b| b == b'\n').count() + 1;

                // Find the line that includes the subslice:
                // Find the *last* newline before the substring starts
                let line_begin = prefix
                    .iter()
                    .rev()
                    .position(|&b| b == b'\n')
                    .map(|pos| offset - pos)
                    .unwrap_or(0);

                // Find the full line after that newline
                let line = input[line_begin..]
                    .lines()
                    .next()
                    .unwrap_or(&input[line_begin..])
                    .trim_end();

                // The (1-indexed) column number is the offset of our substring into that line
                let column_number = line.offset(substring) + 1;

                match kind {
                    Typo::Custom(msg) => write!(
                        &mut result,
                        "Syntax Error #{i}:\n\
                        {space:4}|\n\
                        {line_number:<4}| {line}\n\
                        {space:4}| {caret:>column$}\n\
                        {space:4}= {msg}\n\n",
                        i = i+1,
                        line_number = line_number,
                        line = line,
                        caret = '^',
                        column = column_number,
                        space = "",
                        msg = msg,

                    ),
                    Typo::Expected(chars) => {
                        if let Some(actual) = substring.chars().next() {
                            write!(
                                &mut result,
                                "Syntax Error #{i}: encountered unexpected character\n\
                                {space:4}|\n\
                                {line_number:<4}| {line}\n\
                                {space:4}| {caret:>column$}\n\
                                {space:4}= I expected one of {expected:?}, but found '{actual}'.\n\n",
                                i = i + 1,
                                line_number = line_number,
                                line = line,
                                caret = '^',
                                column = column_number,
                                expected = chars,
                                actual = actual,
                                space = "",
                            )
                        } else {
                            write!(
                                &mut result,
                                "Syntax Error #{i}: end of input expected\n\
                                {space:4}|\n\
                                {line_number:<4}| {line}\n\
                                {space:4}| {caret:>column$}\n\
                                {space:4}= I expected one of {expected:?}, but got end of input.\n\n",
                                i = i + 1,
                                line_number = line_number,
                                line = line,
                                caret = '^',
                                column = column_number,
                                expected = chars,
                                space = "",
                            )
                        }
                    }
                    Typo::Context(s) => write!(
                        &mut result,
                        "Syntax Error #{i}: failure during parse of '{context}'\n\
                        {space:4}|\n\
                        {line_number:<4}| {line}\n\
                        {space:4}| {caret:>column$}\n\
                        {space:4}= I couldn't parse a {context} here.\n\n",
                        i = i + 1,
                        line_number = line_number,
                        context = s,
                        line = line,
                        caret = '^',
                        column = column_number,
                        space = "",
                    ),
                    Typo::Nom(e) => write!(
                        &mut result,
                        "Syntax Error #{i}: subparser '{nom_err:?}' failed\n\
                        {space:4}|\n\
                        {line_number:<4}| {line}\n\
                        {space:4}| {caret:>column$}\n\
                        {space:4}= I needed to use a '{nom_err_desc}' parser here, but it failed.\n\n",
                        i = i + 1,
                        line_number = line_number,
                        nom_err = e,
                        line = line,
                        caret = '^',
                        column = column_number,
                        nom_err_desc = e.description().to_ascii_lowercase(),
                        space = "",
                    ),
                }
            }
            // Because `write!` to a `String` is infallible, this `unwrap` is fine.
            .unwrap();
        }

        result
    }
}
