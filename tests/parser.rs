use libaffix::{gen::parse_grammar, parser::syntax::Grammar};

fn parse_src(src: &str) -> Grammar {
    let mut lexeme_buf = Vec::new();
    parse_grammar(src, &mut lexeme_buf).unwrap_or_else(|err| panic!("{}", err))
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
