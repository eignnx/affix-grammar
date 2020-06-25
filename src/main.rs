mod cli;
use libaffix::{
    checked::CheckedGrammar, fault::DynamicErr, gen::Generator, parser::syntax::ParsedGrammar,
};
use std::convert::{TryFrom, TryInto};
use std::fs;
use std::io::{self, Write};
use structopt::StructOpt;

fn exit_with_error<E: std::fmt::Display>(e: E) -> ! {
    eprintln!("{}", e);
    std::process::exit(-1);
}

fn ask<'buf>(question: impl AsRef<[u8]>, line: &'buf mut String) -> io::Result<&'buf str> {
    io::stdout().write_all(question.as_ref())?;
    io::stdout().flush()?;
    line.clear();
    io::stdin().read_line(line)?;
    Ok(line.trim())
}

fn main() -> io::Result<()> {
    let cli_options = cli::Options::from_args();

    // Read in the grammar, then parse it, then deallocate the text buffer.
    let grammar = {
        let src = fs::read_to_string(&cli_options.grammar_file)?;
        ParsedGrammar::try_from(&src[..]).unwrap_or_else(|err| exit_with_error(err))
    };

    let (resolved_grammar, signatures) = grammar
        .clone()
        .try_into()
        .unwrap_or_else(|err| exit_with_error(err));

    let _checked_grammar: CheckedGrammar = (resolved_grammar, signatures)
        .try_into()
        .unwrap_or_else(|err| exit_with_error(err));

    let mut generator = Generator::new(grammar);

    println!();
    for _ in 0..cli_options.number {
        let sentence = match generator.generate() {
            Ok(sentence) => sentence,
            Err(DynamicErr::MaxTrialsExceeded { .. }) => {
                println!();
                println!(
                    "[Max iterations ({}) exceeded and no new sentences found! \
                     This may mean all possible sentences have been generated.]",
                    generator.max_trials
                );
                break;
            }
            Err(err) => exit_with_error(err),
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
                Ok(sentence) => {
                    println!("\n\t\"{}\"\n", sentence);
                    break 'inner;
                }
                Err(DynamicErr::MaxTrialsExceeded { .. }) => {
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
                Err(err) => exit_with_error(err),
            };
        }

        resp = ask(
            "[Press ENTER to generate another sentence, press ANY OTHER KEY to EXIT.] ",
            &mut line_buf,
        )?;
    }

    Ok(())
}
