mod cli;
mod fault;
mod gen;
mod parser;

use gen::{parse_grammar, Generator};
use std::fs;
use std::io::{self, Write};
use structopt::StructOpt;

fn ask<'buf>(question: impl AsRef<[u8]>, line: &'buf mut String) -> io::Result<&'buf str> {
    io::stdout().write_all(question.as_ref())?;
    io::stdout().flush()?;
    line.clear();
    io::stdin().read_line(line)?;
    Ok(line.trim())
}

fn main() -> io::Result<()> {
    let cli_options = cli::Options::from_args();
    let src = fs::read_to_string(&cli_options.grammar_file)?;
    let mut lexeme_buf = Vec::new();
    let grammar = parse_grammar(&src, &mut lexeme_buf).unwrap_or_else(|err| panic!("{}", err));

    let mut generator = Generator::new(grammar);

    println!();
    for _ in 0..cli_options.number {
        let sentence = match generator.generate() {
            Ok(Some(sentence)) => sentence,
            Ok(None) => {
                println!();
                println!(
                    "[Max iterations ({}) exceeded and no new sentences found! \
                     This may mean all possible sentences have been generated.]",
                    generator.max_trials
                );
                break;
            }
            Err(err) => panic!("{}", err),
        };
        println!("\t\"{}\"", sentence);
    }
    println!();

    let mut line_buf = String::new();
    let mut resp = ask(
        "[Press ENTER to generate another sentence, press ANY OTHER KEY to EXIT.] ",
        &mut line_buf,
    )?;

    'outer: while cli_options.interactive && resp.is_empty() {
        'inner: loop {
            match generator.generate() {
                Ok(Some(sentence)) => {
                    println!("\n\t\"{}\"\n", sentence);
                    break 'inner;
                }
                Ok(None) => {
                    println!(
                        "[Max iterations ({}) exceeded and no new sentences found! \
                         This may mean all possible sentences have been generated.]",
                        generator.max_trials
                    );

                    resp = ask(
                        format!(
                            "[Press ENTER to RAISE MAX ITERATIONS to {} and try again, \
                             press ANY OTHER KEY to EXIT.] ",
                            generator.max_trials * 2
                        ),
                        &mut line_buf,
                    )?;

                    if resp.is_empty() {
                        generator.max_trials *= 2;
                        continue 'inner;
                    } else {
                        println!("[Exiting...]");
                        break 'outer;
                    }
                }
                Err(err) => panic!("{}", err),
            };
        }

        resp = ask(
            "[Press ENTER to generate another sentence, press ANY OTHER KEY to EXIT.] ",
            &mut line_buf,
        )?;
    }

    Ok(())
}
