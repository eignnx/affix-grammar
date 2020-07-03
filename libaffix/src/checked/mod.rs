use crate::fault;
use itertools::Itertools;
use resolve::{DataVariant, ResolvedGrammar, RuleDecl, RuleName, SigMap};
use std::convert::TryFrom;

pub mod resolve;

/// Represents a value `value` that carries along some context `ctx`.
/// Use the `WithContext` trait's `.with_ctx(_)` method to create one of these.
pub struct Ctx<T, C> {
    pub val: T,
    pub ctx: C,
}

pub trait WithCtx<C>: Sized {
    /// Creates a `Ctx` from `self` and some supplied context `ctx`.
    fn with_ctx(self, ctx: C) -> Ctx<Self, C> {
        Ctx { val: self, ctx }
    }
}

impl<T: Sized, C> WithCtx<C> for T {}

/// This is a `ResolvedGrammar` whose `Cases` have all passed exhaustiveness
/// checks.
pub struct CheckedGrammar(pub(crate) ResolvedGrammar);

impl TryFrom<Ctx<ResolvedGrammar, SigMap>> for CheckedGrammar {
    type Error = fault::SemanticErr;

    fn try_from(
        Ctx {
            val: resolved_grammar,
            ctx: rule_sigs,
        }: Ctx<ResolvedGrammar, SigMap>,
    ) -> Result<Self, Self::Error> {
        for (rule_name, rule_decl) in &resolved_grammar.rule_decls {
            let rule_sig = &rule_sigs[rule_name];
            let variant_matrix = resolved_grammar.variant_matrix_from_sig(&rule_sig);
            check_exhaustiveness(rule_name, rule_decl, variant_matrix)?;
        }

        Ok(CheckedGrammar(resolved_grammar))
    }
}

fn check_exhaustiveness(
    rule_name: &RuleName,
    rule_decl: &RuleDecl,
    variant_matrix: Vec<Vec<&DataVariant>>,
) -> fault::SemanticRes {
    if rule_decl.arity() == 0 && rule_decl.cases.len() == 1 {
        return Ok(());
    }

    let mut useful_cases = vec![false; rule_decl.cases.len()];

    // For each tuple in the cartesian product...
    'product_loop: for tuple in variant_matrix.iter().multi_cartesian_product() {
        // Walk through each `Case`...
        for (case_idx, case) in rule_decl.cases.iter().enumerate() {
            // If the `Case` covers the tuple, move on the the next tuple.
            if case.covers(tuple.iter()) {
                useful_cases[case_idx] = true;
                continue 'product_loop;
            }
        }

        // If we have run through all the cases, the rule is in-exhaustive.
        return Err(fault::SemanticErr::InexhaustiveCaseAnalysis {
            rule_name: rule_name.to_string(),
            arguments: tuple.iter().map(ToString::to_string).collect(),
        });
    }

    // Finally, check to make sure all cases were useful.
    if let Some((unused_idx, _)) = useful_cases.iter().find_position(|&&x| !x) {
        let unused_case_number = unused_idx + 1;
        Err(fault::SemanticErr::UnreacheableRuleCase {
            rule_name: rule_name.to_string(),
            unused_case_number,
        })
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::syntax::ParsedGrammar;
    use assert_matches::assert_matches;
    use resolve::SigMap;
    use std::convert::TryInto;

    fn parse_then_resolve(src: &str) -> fault::SemanticRes<Ctx<ResolvedGrammar, SigMap>> {
        let parsed: ParsedGrammar = src.try_into().unwrap();
        parsed.try_into()
    }

    fn check_rule(
        rule_name: impl Into<RuleName>,
        grammar_ctx: Ctx<ResolvedGrammar, SigMap>,
    ) -> fault::SemanticRes {
        let Ctx {
            val: grammar,
            ctx: rule_sigs,
        } = grammar_ctx;
        let rule_name = rule_name.into();
        let rule_decl = &grammar.rule_decls[&rule_name];
        let rule_sig = &rule_sigs[&rule_name];
        let variant_matrix = grammar.variant_matrix_from_sig(rule_sig);
        check_exhaustiveness(&rule_name, rule_decl, variant_matrix)
    }

    #[test]
    fn test_usefullness() {
        let grammar_ctx = parse_then_resolve(
            r#"
            data Abc = a | b | c
            rule start = "X"
            rule test_rule.Abc.Abc =
                .a.a -> "X"
                .a.b -> "X"
                .*.* -> "X"
                .a.c -> "X"
            "#,
        )
        .unwrap();

        assert_matches!(
            check_rule("test_rule", grammar_ctx),
            Err(fault::SemanticErr::UnreacheableRuleCase {
                rule_name,
                unused_case_number: 4,
            }) if &rule_name == "test_rule"
        );
    }

    #[test]
    fn test_exhaustiveness() {
        let grammar_ctx = parse_then_resolve(
            r#"
            data Abc = a | b | c
            rule start = "X"
            rule test_rule.Abc.Abc =
                .a.a -> "X"
                .a.b -> "X"
            "#,
        )
        .unwrap();

        assert_matches!(
            check_rule("test_rule", grammar_ctx),
            Err(fault::SemanticErr::InexhaustiveCaseAnalysis {
                rule_name,
                ..
            }) if &rule_name == "test_rule"
        );
    }

    #[test]
    fn test_0_arity_rule_exhaustiveness_and_usefulness() {
        let grammar_ctx = parse_then_resolve(
            r#"
            rule start = "X" | "Y" | "Z"
            "#,
        )
        .unwrap();

        assert_matches!(check_rule("start", grammar_ctx), Ok(_));
    }
}
