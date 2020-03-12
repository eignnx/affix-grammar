mod cli;
mod gen;
mod parser;

use gen::Generator;
use std::{
    convert::TryFrom,
    io::{self, Write},
};
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
    let mut generator = Generator::try_from(cli_options.grammar_file.as_ref())?;

    println!();
    for _ in 0..cli_options.number {
        let sentence = match generator.generate() {
            Some(sentence) => sentence,
            None => {
                println!();
                println!(
                    "[Max iterations ({}) exceeded and no new sentences found! \
                     This may mean all possible sentences have been generated.]",
                    generator.max_trials
                );
                break;
            }
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
                Some(sentence) => {
                    println!("\n\t\"{}\"\n", sentence);
                    break 'inner;
                }
                None => {
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
            };
        }

        resp = ask(
            "[Press ENTER to generate another sentence, press ANY OTHER KEY to EXIT.] ",
            &mut line_buf,
        )?;
    }

    Ok(())
}
