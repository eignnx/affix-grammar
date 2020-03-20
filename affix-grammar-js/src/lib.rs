use libaffix::gen::{parse_grammar, Generator};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub struct ParserContext {
    generator: Generator,
}

#[wasm_bindgen]
impl ParserContext {
    #[wasm_bindgen(constructor)]
    pub fn new(src: String) -> Result<ParserContext, JsValue> {
        let mut lexeme_buf = Vec::new();
        let grammar = parse_grammar(&src, &mut lexeme_buf)?;
        let generator = Generator::new(grammar);
        Ok(ParserContext { generator })
    }

    #[wasm_bindgen]
    pub fn generate(&mut self) -> Result<String, JsValue> {
        match self.generator.generate()? {
            Some(sentence) => Ok(sentence),
            None => Err("Max iterations execeeded.".into()),
        }
    }

    #[wasm_bindgen(setter = maxTrials)]
    pub fn set_max_trials(&mut self, trials: usize) {
        self.generator.max_trials = trials;
    }

    #[wasm_bindgen(getter = maxTrials)]
    pub fn max_trials(&self) -> usize {
        self.generator.max_trials
    }
}
