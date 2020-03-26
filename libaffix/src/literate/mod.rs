use nom::{bytes::complete::tag, re_find, IResult};

#[derive(Debug)]
enum Line<'src> {
    Comment(&'src str),
    Code(&'src str),
    Blank,
}

fn comment(line: &str) -> IResult<&str, &str> {
    tag("-- ")(line)
}

fn blank(line: &str) -> IResult<&str, &str> {
    re_find!(line, r"^[ \t]*$")
}

impl<'src> Line<'src> {
    fn classify(line: &'src str) -> Self {
        if let Ok((content, _dashes)) = comment(line) {
            Self::Comment(content)
        } else if let Ok(_) = blank(line) {
            Self::Blank
        } else {
            Line::Code(line)
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Block {
    Explanation(String),
    Code(String),
}

pub fn parse<'src>(src: &'src str, buf: &mut Vec<Block>) {
    let lines = src.lines();
    let mut current_block = None;
    for line in lines {
        match Line::classify(line) {
            Line::Blank => {
                if let Some(block) = current_block.take() {
                    buf.push(block);
                }
            }
            Line::Comment(line) => match &mut current_block {
                Some(Block::Explanation(lines)) => {
                    lines.push(' ');
                    lines.push_str(line);
                }
                current_block => {
                    let new_block = Block::Explanation(line.to_string());
                    if let Some(block) = current_block.replace(new_block) {
                        buf.push(block);
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
                        buf.push(block);
                    }
                }
            },
        }
    }

    // Push the final block into the buffer.
    if let Some(block) = current_block.take() {
        buf.push(block);
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
"#,
        &mut buf,
    );

    assert_eq!(
        buf,
        vec![
            Block::Explanation("this is a single block.".into()),
            Block::Code("rule foo\n  = bar\n  | baz".into()),
            Block::Explanation("another block.".into())
        ]
    );
}
