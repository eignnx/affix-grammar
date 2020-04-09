use nom::{
    error::{VerboseError, VerboseErrorKind},
    Offset,
};
use std::fmt::Write;

pub fn report_error(input: &str, e: VerboseError<&str>) -> String {
    let mut result = String::new();

    for (i, (substring, kind)) in e.errors.iter().enumerate() {
        let offset = input.offset(substring);

        if input.is_empty() {
            match kind {
                VerboseErrorKind::Char(c) => write!(
                    &mut result,
                    "{}: I expected a '{}', but got empty input.\n\n",
                    i+1, c
                ),
                VerboseErrorKind::Context(s) => write!(
                    &mut result,
                    "{}: I was trying to parse a {}, but got empty input.\n\n",
                    i+1, s
                ),
                VerboseErrorKind::Nom(e) => write!(
                    &mut result,
                    "{}: I was trying to parse a {:?}, but got empty input.\n\n",
                    i+1, e
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
                VerboseErrorKind::Char(c) => {
                    if let Some(actual) = substring.chars().next() {
                        write!(
                            &mut result,
                            "Syntax Error #{i}: encountered unexpected character\n\
                            {space:4}|\n\
                            {line_number:<4}| {line}\n\
                            {space:4}| {caret:>column$}\n\
                            {space:4}= I expected something like '{expected}', but found '{actual}'.\n\n",
                            i = i+1,
                            line_number = line_number,
                            line = line,
                            caret = '^',
                            column = column_number,
                            expected = c,
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
                            {space:4}= I expected something like '{expected}', but got end of input.\n\n",
                            i = i + 1,
                            line_number = line_number,
                            line = line,
                            caret = '^',
                            column = column_number,
                            expected = c,
                            space = ""
                        )
                    }
                }
                VerboseErrorKind::Context(s) => write!(
                    &mut result,
                    "Syntax Error #{i}: failure during parse of '{context}'\n\
                    {space:4}|\n\
                    {line_number:<4}| {line}\n\
                    {space:4}| {caret:>column$}\n\
                    {space:4}= I couldn't parse a {context} here.\n\n",
                    i = i+1,
                    line_number = line_number,
                    context = s,
                    line = line,
                    caret = '^',
                    column = column_number,
                    space = "",
                ),
                VerboseErrorKind::Nom(e) => write!(
                    &mut result,
                    "Syntax Error #{i}: subparser '{nom_err:?}' failed\n\
                    {space:4}|\n\
                    {line_number:<4}| {line}\n\
                    {space:4}| {caret:>column$}\n\
                    {space:4}= I needed to use a '{nom_err_desc}' parser here, but it failed.\n\n",
                    i = i+1,
                    line_number = line_number,
                    nom_err = e,
                    line = line,
                    caret = '^',
                    column = column_number,
                    nom_err_desc = e.description().to_ascii_lowercase(),
                    space = ""
                ),
            }
        }
        // Because `write!` to a `String` is infallible, this `unwrap` is fine.
        .unwrap();
    }

    result
}
