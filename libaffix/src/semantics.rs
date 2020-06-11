use crate::fault;
use crate::parser::syntax::Grammar;

pub fn analyze(grammar: Grammar) -> fault::StaticRes<'static, Grammar> {
    let mut new_grammar = Grammar::default();
    for rule_decl in grammar.rule_decls {
        for abbr_data_name in rule_decl.signature.parameter_types {
            // let data_name = grammar.data_decl_from_abbr_variable(abbr_data_name)?;
        }
    }
    Ok(new_grammar)
}
