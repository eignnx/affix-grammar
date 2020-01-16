mod cli;
mod gen;
mod parse;
mod syntax;

use gen::Generator;
use std::{
    convert::TryFrom,
    io::{self, Write},
};
use structopt::StructOpt;

fn main() -> io::Result<()> {
    let cli_options = cli::Options::from_args();
    let mut generator = Generator::try_from(cli_options.grammar_file.as_ref())?;

    for _ in 0..cli_options.number {
        let sentence = generator.generate().expect("Max iterations exceeded!");
        println!("\"{}\"", sentence);
    }

    let mut line = String::with_capacity(80);
    while cli_options.interactive {
        line.clear();
        print!("[Press ENTER to generate another sentence, press ANY OTHER KEY to quit.] ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut line)?;
        if line.trim().is_empty() {
            let sentence = generator.generate().expect("Max iterations exceeded!");
            println!("\"{}\"", sentence);
        } else {
            println!("[Exiting...]");
            break;
        }
    }

    Ok(())
}
