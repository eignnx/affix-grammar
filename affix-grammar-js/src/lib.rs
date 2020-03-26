#[macro_use]
extern crate serde_derive;

use libaffix::{
    gen::{parse_grammar, Generator},
    literate,
};
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
    pub fn new(src: &str) -> Result<ParserContext, JsValue> {
        let mut lexeme_buf = Vec::new();
        let grammar = parse_grammar(src, &mut lexeme_buf)?;
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

#[wasm_bindgen]
pub struct LiterateParser {
    blocks: Vec<literate::Block>,
    index: usize,
}

#[wasm_bindgen]
impl LiterateParser {
    #[wasm_bindgen(constructor)]
    pub fn new(src: &str) -> Self {
        let mut blocks = vec![];
        literate::parse(src, &mut blocks);
        Self { blocks, index: 0 }
    }

    #[wasm_bindgen]
    pub fn reset(&mut self) {
        self.index = 0;
    }

    #[wasm_bindgen]
    pub fn next(&mut self) -> JsValue {
        let done = self.index >= self.blocks.len();
        let iter_val = if !done {
            let block = &self.blocks[self.index];
            self.index += 1;
            BlockIterValue {
                value: Some(block),
                done,
            }
        } else {
            BlockIterValue { value: None, done }
        };

        let msg = "cannot convert from Block to JsValue";
        serde_wasm_bindgen::to_value(&iter_val).expect(msg)
    }
}

#[derive(Serialize)]
pub struct BlockIterValue<'a> {
    pub value: Option<&'a literate::Block>,
    pub done: bool,
}
