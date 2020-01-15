mod parse;
mod syntax;

use rand::thread_rng;
use std::{
    cell::RefCell,
    collections::HashMap,
    io::{self, Read},
    rc::Rc,
};
use string_interner::StringInterner;

fn main() -> io::Result<()> {
    let mut src = String::new();
    io::stdin().read_to_string(&mut src)?;
    let symbol_pool = Rc::new(RefCell::new(StringInterner::default()));
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

    let sentence = grammar.generate(symbol_pool.clone(), &mut thread_rng(), &mut HashMap::new());
    println!("{:?}", sentence);

    Ok(())
}
