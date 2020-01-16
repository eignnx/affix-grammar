mod cli;
mod parse;
mod syntax;

use rand::thread_rng;
use std::{
    fs,
    io::{self, Read, Write},
};
use structopt::StructOpt;
use syntax::{make_symbol_pool, Grammar, Syms};

fn parse_grammar(cli_options: &cli::Options) -> io::Result<(Grammar, Syms)> {
    let mut src = String::new();
    let mut file = fs::File::open(&cli_options.grammar_file)?;
    file.read_to_string(&mut src)?;

    let symbol_pool = make_symbol_pool();
    let parse_res = parse::parse_source(&src, symbol_pool.clone());

    let grammar = match parse_res {
        Ok((_, grammar)) => grammar,
        Err(nom::Err::Error((rest, e))) => {
            eprintln!("Could not parse source text! Got error: {:?}", e);
            eprintln!("Parsed up until this point:```\n{}\n```", rest);
            std::process::exit(-1);
        }
        _ => unimplemented!(),
    };

    Ok((grammar, symbol_pool))
}

fn main() -> io::Result<()> {
    let cli_options = cli::Options::from_args();
    let (grammar, symbol_pool) = parse_grammar(&cli_options)?;
    let mut rng = thread_rng();

    for _ in 0..cli_options.number {
        let sentence = grammar.generate(symbol_pool.clone(), &mut rng, &mut im::HashMap::new());
        println!("\"{}\"", sentence);
    }

    let mut line = String::with_capacity(80);
    while cli_options.interactive {
        line.clear();
        print!("[Press ENTER to generate another sentence, press ANY OTHER KEY to quit.] ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut line)?;
        if line.trim().is_empty() {
            let sentence = grammar.generate(symbol_pool.clone(), &mut rng, &mut im::HashMap::new());
            println!("\"{}\"", sentence);
        } else {
            println!("[Exiting...]");
            break;
        }
    }

    Ok(())
}
