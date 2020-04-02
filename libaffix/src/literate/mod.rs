use nom::{bytes::complete::tag, character::complete::char, combinator::opt, re_find, IResult};
use pulldown_cmark::{self as cmark};

#[derive(Debug)]
enum Line<'src> {
    Comment(&'src str),
    Code(&'src str),
    Blank,
}

/// Parses `--` at beginning of line, then optionally (hopfully) a space `' '`.
/// Then the rest is the possibly-empty content of the comment. This allows the
/// line `--\n` to be parsed as an empty comment, while still stripping off the
/// leading space character of a non-empty comment.
fn comment(line: &str) -> IResult<&str, &str> {
    let (rest, _dashes) = tag("--")(line)?;
    let (rest, _maybe_space) = opt(char(' '))(rest)?;
    Ok(("", rest)) // Note: we're returning the `rest` of the previous parser!
}

#[test]
fn empty_comment() {
    let (_rest, content) = comment("--").unwrap();
    assert_eq!(content, "");
}

#[test]
fn strip_leading_space_from_comment() {
    let (_rest, content) = comment("-- Word.").unwrap();
    assert_eq!(content, "Word.");
}

fn blank(line: &str) -> IResult<&str, &str> {
    re_find!(line, r"^[ \t]*$")
}

impl<'src> Line<'src> {
    fn classify(line: &'src str) -> Self {
        if let Ok((_rest, content)) = comment(line) {
            Self::Comment(content)
        } else if let Ok(_) = blank(line) {
            Self::Blank
        } else {
            Line::Code(line)
        }
    }
}

#[derive(Debug, PartialEq, Serialize)]
pub enum Block {
    Explanation(String),
    Code(String),
}

impl Block {
    fn map_explanation(self, f: impl Fn(String) -> String) -> Self {
        match self {
            Self::Explanation(explanation) => Self::Explanation(f(explanation)),
            Self::Code(code) => Self::Code(code),
        }
    }
}

fn to_html(md: String) -> String {
    let mut options = cmark::Options::empty();
    options.insert(cmark::Options::ENABLE_STRIKETHROUGH);
    options.insert(cmark::Options::ENABLE_TASKLISTS);
    options.insert(cmark::Options::ENABLE_FOOTNOTES);
    let renderer = cmark::Parser::new_ext(&md, options);
    let mut output = String::new();
    cmark::html::push_html(&mut output, renderer);
    output
}

pub fn parse<'src>(src: &'src str, buf: &mut Vec<Block>) {
    let lines = src.lines();
    let mut current_block: Option<Block> = None;
    for line in lines {
        match Line::classify(line) {
            Line::Blank => {
                if let Some(block) = current_block.take() {
                    buf.push(block.map_explanation(to_html));
                }
            }
            Line::Comment(line) => match &mut current_block {
                Some(Block::Explanation(lines)) => {
                    lines.push('\n'); // Keep the \n b/c markdown renderer needs it.
                    lines.push_str(line);
                }
                current_block => {
                    let new_block = Block::Explanation(line.to_string());
                    if let Some(block) = current_block.replace(new_block) {
                        buf.push(block.map_explanation(to_html));
                    }
                }
            },
            Line::Code(line) => match &mut current_block {
                Some(Block::Code(lines)) => {
                    lines.push('\n');
                    lines.push_str(line);
                }
                current_block => {
                    let new_block = Block::Code(line.to_string());
                    if let Some(block) = current_block.replace(new_block) {
                        buf.push(block.map_explanation(to_html));
                    }
                }
            },
        }
    }

    // Push the final block into the buffer.
    if let Some(block) = current_block.take() {
        buf.push(block.map_explanation(to_html));
    }
}

#[test]
fn ensure_blank_line_separates_explanation_blocks() {
    let mut buf = vec![];
    parse(
        r#"
-- this
-- is
-- a single block.
rule foo
  = bar
  | baz
        
-- another block.

-- This is a [link](https://www.google.com)!
"#,
        &mut buf,
    );

    assert_eq!(
        buf,
        vec![
            Block::Explanation("<p>this\nis\na single block.</p>\n".into()),
            Block::Code("rule foo\n  = bar\n  | baz".into()),
            Block::Explanation("<p>another block.</p>\n".into()),
            Block::Explanation(
                "<p>This is a <a href=\"https://www.google.com\">link</a>!</p>\n".into()
            )
        ]
    );
}
