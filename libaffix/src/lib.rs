#[macro_use]
extern crate serde_derive;

pub mod checked;
pub mod fault;
pub mod gen;
pub mod literate;
pub mod parser;

#[cfg(test)]
mod tests {
    use super::*;
    use std::convert::{TryFrom, TryInto};
    use std::fs;
    use std::path::PathBuf;

    static RNG_SEED: u64 = 123456;
    static MAX_VALUES: usize = 100;

    #[test]
    fn run_snapshots() {
        let inputs_dir = PathBuf::from(std::env!("CARGO_MANIFEST_DIR"))
            .join("src")
            .join("snapshot_inputs");

        for dir_entry in fs::read_dir(inputs_dir).unwrap() {
            let path = dir_entry.unwrap().path();
            let src_text = fs::read_to_string(&path).unwrap();
            let parse_res = parser::syntax::ParsedGrammar::try_from(&src_text[..]);
            let filename = path.file_stem().unwrap().to_string_lossy();

            let parse_snapshot_name = format!("PARSE_SNAPSHOT__{}", filename,);
            insta::assert_json_snapshot!(parse_snapshot_name, parse_res);

            if let Some(grammar) = parse_res.ok() {
                let resolve_snapshot_name = format!("RESOLVE_SNAPSHOT__{}", filename);
                let resolve_res = grammar.clone().try_into();

                match resolve_res {
                    Err(err) => {
                        insta::assert_display_snapshot!(resolve_snapshot_name, err);
                    }
                    Ok((resolved_grammar, signatures)) => {
                        insta::assert_display_snapshot!(resolve_snapshot_name, "✅");

                        // Now run the exhaustiveness and usefullness checks.
                        let checked_snapshot_name = format!("CHECKED_SNAPSHOT__{}", filename);
                        let checked_res: fault::DynamicRes<checked::CheckedGrammar> =
                            (resolved_grammar, signatures).try_into();

                        match checked_res {
                            Err(err) => {
                                insta::assert_display_snapshot!(checked_snapshot_name, err);
                            }
                            Ok(_checked_grammar) => {
                                insta::assert_display_snapshot!(checked_snapshot_name, "✅");

                                // No generate some sentences.
                                let generator = gen::Generator::new_seeded(grammar, RNG_SEED);
                                let generated = generate_n(generator, MAX_VALUES);
                                let gen_snapshot_name = format!("GEN_SNAPSHOT__{}", filename);
                                insta::assert_json_snapshot!(gen_snapshot_name, generated);
                            }
                        }
                    }
                }
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
