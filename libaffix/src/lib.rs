#[macro_use]
extern crate serde_derive;

pub mod fault;
pub mod gen;
pub mod literate;
pub mod parser;
pub mod semantics;

#[cfg(test)]
mod tests {
    use super::*;
    use std::convert::TryFrom;
    use std::fs;
    use std::path::PathBuf;

    static RNG_SEED: u64 = 123456;
    static MAX_VALUES: usize = 100;

    #[test]
    fn run_snapshots() {
        let inputs = PathBuf::from(std::env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join("snapshot_inputs");

        for dir_entry in fs::read_dir(inputs).unwrap() {
            let path = dir_entry.unwrap().path();
            let src_text = fs::read_to_string(&path).unwrap();
            let parse_res = parser::syntax::ParsedGrammar::try_from(&src_text[..]);

            let parse_snapshot_name = format!(
                "PARSE_SNAPSHOT__{}",
                path.file_stem().unwrap().to_string_lossy()
            );
            insta::assert_json_snapshot!(parse_snapshot_name, parse_res);

            if let Some(grammar) = parse_res.ok() {
                let generator = gen::Generator::new_seeded(grammar, RNG_SEED);
                let generated = generate_n(generator, MAX_VALUES);
                let gen_snapshot_name = format!(
                    "GEN_SNAPSHOT__{}",
                    path.file_stem().unwrap().to_string_lossy()
                );
                insta::assert_json_snapshot!(gen_snapshot_name, generated);
            }
        }
    }

    fn generate_n<R: rand::Rng>(
        mut gen: gen::Generator<R>,
        n: usize,
    ) -> fault::DynamicRes<Vec<String>> {
        let mut sentences = vec![];
        for _ in 0..n {
            match gen.generate() {
                Ok(sentence) => sentences.push(sentence),
                Err(fault::DynamicErr::MaxTrialsExceeded { .. }) => return Ok(sentences),
                Err(e) => return Err(e),
            }
        }
        Ok(sentences)
    }
}
