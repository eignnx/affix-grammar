#[derive(Debug, Clone)]
pub enum Typo {
    Custom(&'static str, String),
    Context(&'static str),
    Expected(Vec<char>),
    Nom(nom::error::ErrorKind),
}

#[derive(Debug, Clone)]
pub struct SourcedTypo<I> {
    pub fragment: I,
    pub typo: Typo,
}

#[derive(Debug, Clone, Default)]
pub struct Report<I> {
    typos: Vec<SourcedTypo<I>>,
}

impl<I> From<SourcedTypo<I>> for Report<I> {
    fn from(sourced: SourcedTypo<I>) -> Self {
        Self {
            typos: vec![sourced],
        }
    }
}

impl<I> nom::error::ParseError<I> for Report<I> {
    fn from_error_kind(fragment: I, kind: nom::error::ErrorKind) -> Self {
        Self::from(SourcedTypo {
            fragment,
            typo: Typo::Nom(kind),
        })
    }

    fn append(fragment: I, kind: nom::error::ErrorKind, mut other: Self) -> Self {
        other.typos.push(SourcedTypo {
            fragment,
            typo: Typo::Nom(kind),
        });
        other
    }

    fn from_char(fragment: I, c: char) -> Self {
        Self::from(SourcedTypo {
            fragment,
            typo: Typo::Expected(vec![c]),
        })
    }

    /// If both `self` and `other` have `Typo::Expected` variants as their most
    /// recent typo, combine them. Otherwise, merge `self` and `other` and
    /// return `self`.
    fn or(mut self, mut other: Self) -> Self {
        match (self.typos.last_mut(), other.typos.last_mut()) {
            (Some(SourcedTypo { typo: my, .. }), Some(SourcedTypo { typo: ur, .. })) => {
                if let (Typo::Expected(my), Typo::Expected(ur)) = (my, ur) {
                    my.append(ur);
                    self
                } else {
                    self.typos.append(&mut other.typos);
                    self
                }
            }
            _ => {
                self.typos.append(&mut other.typos);
                self
            }
        }
    }

    fn add_context(fragment: I, ctx: &'static str, mut other: Self) -> Self {
        other.typos.push(SourcedTypo {
            fragment,
            typo: Typo::Context(ctx),
        });
        other
    }
}

#[derive(Debug, Serialize)]
pub struct Loc<'i> {
    pub line: &'i str,
    pub line_no: usize,
    pub column_no: usize,
}

impl<'i> Loc<'i> {
    fn from_fragment(src: &'i str, fragment: &'i str) -> Option<Self> {
        use nom::Offset;
        if src.is_empty() {
            None
        } else {
            let offset = src.offset(fragment);
            let prefix = &src.as_bytes()[..offset];

            // Count the number of newlines in the first `offset` bytes of input
            let line_no = prefix.iter().filter(|&&b| b == b'\n').count() + 1;

            // Find the line that includes the subslice:
            // Find the *last* newline before the substring starts
            let line_begin = prefix
                .iter()
                .rev()
                .position(|&b| b == b'\n')
                .map(|pos| offset - pos)
                .unwrap_or(0);

            // Find the full line after that newline
            let line = src[line_begin..]
                .lines()
                .next()
                .unwrap_or(&src[line_begin..])
                .trim_end();

            // The (1-indexed) column number is the offset of our substring into that line
            let column_no = line.offset(fragment) + 1;

            Some(Loc {
                line,
                line_no,
                column_no,
            })
        }
    }
}

#[derive(Debug, Serialize)]
pub struct ErrorSummary<'i> {
    pub title: Option<&'static str>,
    pub msg: String,
    pub loc: Option<Loc<'i>>,
    pub trace: Vec<String>,
}

fn primary_summary_error<'i>(
    src: &'i str,
    SourcedTypo { typo, fragment }: SourcedTypo<&'i str>,
    trace: Vec<String>,
) -> ErrorSummary<'i> {
    match Loc::from_fragment(src, fragment) {
        None => ErrorSummary {
            title: Some("EMPTY SOURCE FILE"),
            msg: "I got an empty grammar. Why not try 'rule start = \"Hi!\"'.".into(),
            loc: None,
            trace,
        },
        loc @ Some(_) => match typo {
            Typo::Custom(title, msg) => ErrorSummary {
                title: Some(title),
                msg,
                loc,
                trace,
            },
            Typo::Context(ctx) => ErrorSummary {
                title: Some("CONTEXTUAL TRACE"),
                msg: format!(
                    "I was attempting to parse {} here when I encountered a \
                    problem.",
                    ctx
                ),
                loc,
                trace,
            },
            Typo::Expected(char_set) => ErrorSummary {
                title: Some("UNEXPECTED CHARACTER"),
                msg: format!(
                    "I was expecting one of the following characters here: {:?}",
                    char_set,
                ),
                loc,
                trace,
            },
            Typo::Nom(nom_err) => ErrorSummary {
                title: Some("INTERNAL PARSER ERROR"),
                msg: format!(
                    "Sorry, I'm not sure what happened. I got this error: {:?}",
                    nom_err,
                ),
                loc,
                trace,
            },
        },
    }
}

/// Creates an abridged version of an `ErrorSummary` suitable for use in an
/// `ErrorSummary`'s `trace` field.
/// ## Arguments
/// - `src`: the `&'i str` containing the entire source file.
/// - `fragment`: the `&'i str` starting at the place where the parser failed.
fn secondary_summary_error<'i>(
    src: &'i str,
    SourcedTypo { typo, fragment }: &SourcedTypo<&'i str>,
) -> Option<String> {
    match Loc::from_fragment(src, fragment) {
        None => None,
        Some(loc) => match typo {
            Typo::Custom(title, _msg) => Some(format!(
                "...after parsing a problematic {} at line {} column {}",
                title, loc.line_no, loc.column_no
            )),
            Typo::Context(ctx) => Some(format!(
                "...while parsing {} at line {} column {}",
                ctx, loc.line_no, loc.column_no
            )),
            Typo::Expected(char_set) => Some(format!(
                "...when I expected one of {:?} at line {} column {}",
                char_set, loc.line_no, loc.column_no
            )),
            Typo::Nom(nom_err) => Some(format!(
                "...after trying to apply the following internal parser at \
                line {} column {}: {:?}",
                loc.line_no, loc.column_no, nom_err
            )),
        },
    }
}

fn split_first_owned<T>(mut v: Vec<T>) -> Option<(T, Vec<T>)> {
    if v.len() == 0 {
        None
    } else {
        let tail = v.split_off(1);
        let head = v.drain(..).next().unwrap();
        Some((head, tail))
    }
}

#[test]
fn test_split_first_owned() {
    let v: Vec<String> = vec!["A".into(), "B".into(), "C".into()];
    assert_eq!(
        split_first_owned(v),
        Some(("A".into(), vec!["B".into(), "C".into()]))
    );
}

impl<'i> Report<&'i str> {
    pub fn summarize(self, src: &'i str) -> ErrorSummary<'i> {
        let msg = "ErrorSummary.trace will always be non-empty.";
        let (primary, secondaries) = split_first_owned(self.typos).expect(msg);
        let trace = secondaries
            .into_iter()
            .filter_map(|sourced_typo| secondary_summary_error(src, &sourced_typo))
            .collect();
        primary_summary_error(src, primary, trace)
    }
}
