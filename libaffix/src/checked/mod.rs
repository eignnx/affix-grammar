use crate::fault;
use itertools::Itertools;
use resolve::{DataName, DataVariant, ResolvedGrammar, RuleDecl, RuleName, SignatureMap};
use std::convert::TryFrom;

pub mod resolve;

/// Represents a value `value` that carries along some context `ctx`.
/// Use the `WithContext` trait's `.with_ctx(_)` method to create one of these.
pub struct Ctx<T, C> {
    pub value: T,
    pub ctx: C,
}

pub trait WithCtx<C>: Sized {
    /// Creates a `Ctx` from `self` and some supplied context `ctx`.
    fn with_ctx(self, ctx: C) -> Ctx<Self, C> {
        Ctx { value: self, ctx }
    }
}

impl<T: Sized, C> WithCtx<C> for T {}

/// This is a `ResolvedGrammar` whose `Cases` have all passed exhaustiveness
/// checks.
pub struct CheckedGrammar(pub(crate) ResolvedGrammar);

impl TryFrom<Ctx<ResolvedGrammar, SignatureMap>> for CheckedGrammar {
    type Error = fault::SemanticErr;

    fn try_from(
        Ctx {
            value: resolved_grammar,
            ctx: rule_sigs,
        }: Ctx<ResolvedGrammar, SignatureMap>,
    ) -> Result<Self, Self::Error> {
        for (rule_name, rule_decl) in &resolved_grammar.rule_decls {
            let rule_sig = &rule_sigs[rule_name];
            let variant_matrix = variant_matrix_from_sig(&rule_sig, &resolved_grammar);
            check_exhaustiveness(rule_name, rule_decl, variant_matrix)?;
        }

        Ok(CheckedGrammar(resolved_grammar))
    }
}

/// Create a matrix where each column contains all the `DataVariant`s of a
/// given `DataDecl`. The columns correspond to the types of the parameters
/// to the `RuleDecl`.
fn variant_matrix_from_sig<'variants>(
    rule_sig: &[DataName],
    grammar: &'variants ResolvedGrammar,
) -> Vec<Vec<&'variants DataVariant>> {
    rule_sig
        .iter()
        .map(|param_type| {
            let data_decl = &grammar.data_decls[param_type];
            data_decl.variants.keys().collect()
        })
        .collect()
}

fn check_exhaustiveness(
    rule_name: &RuleName,
    rule_decl: &RuleDecl,
    variant_matrix: Vec<Vec<&DataVariant>>,
) -> fault::SemanticRes<()> {
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

#[test]
fn test_exhaustiveness_and_usefullness() {
    use internship::IStr;
    use std::ops::Deref;

    let row = |slice: &[&str]| -> Vec<DataVariant> {
        slice
            .into_iter()
            .map(Deref::deref)
            .map(IStr::new)
            .map(DataVariant)
            .collect()
    };

    let v = vec![row(&["a", "b", "c"]), row(&["1", "2", "3"])];

    let case = |requirements| resolve::Case {
        requirements,
        alternatives: vec![],
    };

    use resolve::Pattern as P;
    use resolve::{DataName, DataVariable};

    let variant = |s: &str| P::Variant(DataVariant(IStr::new(s)));

    #[allow(unused_variables)]
    let variable = |s: &str| {
        P::Variable(DataVariable {
            data_name: DataName(IStr::new(s)),
            number: IStr::new(""),
        })
    };

    let rule_decl = resolve::RuleDecl {
        cases: vec![
            case(vec![variant("a"), variant("1")]),
            case(vec![P::Star, variant("3")]),
            case(vec![variant("b"), variant("1")]),
            case(vec![P::Star, P::Star]),
            case(vec![variant("b"), P::Star]),
            case(vec![variant("a"), P::Star]),
        ],
    };

    let matrix = v.iter().map(|row| row.iter().collect()).collect();

    let rule_name = RuleName(IStr::new("test_rule"));
    let res = check_exhaustiveness(&rule_name, &rule_decl, matrix);
    dbg!(res);
}
