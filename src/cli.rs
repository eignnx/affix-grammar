use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "state-grammar")]
pub struct Options {
    /// The path to the grammar file.
    #[structopt(name = "GRAMMAR-FILE", parse(from_os_str))]
    pub grammar_file: PathBuf,

    /// The number of sentences to generate. Defaults to 1 if not specified.
    #[structopt(short, long, default_value = "1")]
    pub number: usize,

    /// If this flag is passed in, `state-grammar` will allow you to generate
    /// sentences interactively and on demand.
    #[structopt(short, long)]
    pub interactive: bool,
}
