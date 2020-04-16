use libaffix::parser::syntax::Grammar;
use std::convert::TryFrom;

fn parse_src(src: &str) -> Grammar {
    Grammar::try_from(src).unwrap_or_else(|err| panic!("{}", err))
}

#[test]
fn basic_parse() {
    parse_src(
        r#"
        data Number = singular | plural
        rule start = "test"
        "#,
    );
}

#[test]
fn data_variable_and_variant_in_sentential_form() {
    parse_src(
        r#"
        data Data = one | two | three
        rule start = .Data .one .two .three
        "#,
    );
}
