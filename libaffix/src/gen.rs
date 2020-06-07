use crate::fault::{DynamicErr, DynamicRes};
use crate::parser::syntax::{
    abbreviates, Argument, Case, DataDecl, DataName, DataVariable, DataVariant, Grammar, Guard,
    Pattern, RuleName, RuleRef, Token,
};
use im::{vector, Vector};
use internship::IStr;
use rand::seq::IteratorRandom;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

type State = HashMap<DataVariable, DataVariant>;

pub struct Generator {
    grammar: Grammar,
    rng: RefCell<rand::rngs::ThreadRng>,
    seen_sentences: HashSet<im::Vector<OutToken>>,
    pub max_trials: usize,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
enum OutToken {
    Sym(IStr),
    Plus, // For concatenation without space insertion.
}

impl From<OutToken> for Token {
    fn from(out_token: OutToken) -> Self {
        match out_token {
            OutToken::Sym(sym) => Token::StrLit(sym),
            OutToken::Plus => Token::Plus,
        }
    }
}

impl From<&OutToken> for Token {
    fn from(out_token: &OutToken) -> Self {
        out_token.clone().into()
    }
}

impl From<IStr> for OutToken {
    fn from(sym: IStr) -> Self {
        Self::Sym(sym)
    }
}

impl From<&IStr> for OutToken {
    fn from(sym: &IStr) -> Self {
        Self::Sym(sym.clone())
    }
}

const DEFAULT_MAX_TRIALS: usize = 1000;

impl Generator {
    pub fn new(grammar: Grammar) -> Self {
        Self {
            grammar,
            rng: RefCell::new(rand::thread_rng()),
            seen_sentences: HashSet::new(),
            max_trials: DEFAULT_MAX_TRIALS,
        }
    }

    fn choose_rule<'gen, 'st>(
        &'gen self,
        rule_name: &RuleName,
        arguments: &Vec<DataVariant>,
        state: &'st mut State,
    ) -> DynamicRes<Case> {
        // Collect all rules that *could* expand `token`.
        let rule = self
            .grammar
            .rule_decls
            .iter()
            .filter(|rule_decl| &rule_decl.signature.name == rule_name)
            .next() // TODO: Should this *only* return the first-found rule decl?
            .ok_or_else(|| DynamicErr::unbound_rule_name(rule_name.as_ref()))?;

        // Check each case one after another until an allowable case is found.
        for case in &rule.cases {
            let param_typs = &rule.signature.parameter_types;
            let reqs = &case.guard;
            if let Some(new_state) = self.allowable(rule_name, &param_typs, reqs, arguments)? {
                state.extend(
                    new_state
                        .iter()
                        .map(|(variable, variant)| (variable.clone(), variant.clone())),
                );
                return Ok(case.clone());
            }
        }

        // We are out of possibilities. This case must not have been handled in the case analysis.
        Err(DynamicErr::inexhaustive_case_analysis(
            &self.grammar.rule_decls,
            rule_name,
            arguments,
        ))
    }

    fn allowable<'st>(
        &self,
        rule_name: &RuleName,
        _types: &Vec<DataName>,
        guard: &Guard,
        arguments: &Vec<DataVariant>,
    ) -> DynamicRes<Option<State>> {
        // TODO: add type checking here? When ready, use the currently-unused `_types` parameter.
        if guard.requirements.len() != arguments.len() {
            let arguments = arguments.iter().map(|val| val.to_string()).collect();
            return Err(DynamicErr::WrongArityRuleReference {
                rule_name: rule_name.to_string(),
                arguments,
                expected_len: guard.requirements.len(),
            });
        }

        let mut state = State::new();

        for (req, arg) in guard.requirements.iter().zip(arguments.iter()) {
            match req {
                Pattern::Star => {
                    // Pattern::Star (`.*`) matches against any actual variant.
                    continue;
                }
                Pattern::Variant(patt_variant) => {
                    // This assumes that `arg` is the non-abbreviated form of a
                    // `DataVariant`.
                    if !abbreviates(patt_variant.as_ref(), arg.as_ref()) {
                        return Ok(None);
                    }
                }
                Pattern::Variable(var) => {
                    let variable_decl = self.data_decl_from_abbr_variable(var)?;
                    if !variable_decl.variants.contains_key(arg) {
                        let pattern_type = variable_decl.name.to_string();
                        let (variant_decl, _variant) = self.data_decl_from_abbr_variant(arg)?;
                        let argument_type = variant_decl.name.to_string();
                        return Err(DynamicErr::PatternMatchTypeError {
                            pattern_type,
                            argument_type,
                            pattern_variable: var.to_string(),
                        });
                    }
                    let overwritten = state.insert(var.clone(), arg.clone());
                    if overwritten.is_some() {
                        panic!("Shouldn't be overwriting variable bindings here.");
                    }
                }
            }
        }

        Ok(Some(state))
    }

    /// Given a `DataVariable`, this function will perform a lookup in the
    /// grammar and return the `DataDecl` that the variable refers to. Fails if
    /// no `DataDecl` matches the variable, or if the variable is ambiguous and
    /// could refer to multiple `DataDecl`s.
    fn data_decl_from_abbr_variable<'grammar>(
        &'grammar self,
        var: &DataVariable,
    ) -> DynamicRes<&'grammar DataDecl> {
        let mut matches = self
            .grammar
            .data_decls
            .iter()
            .filter(|decl| decl.name.matches_variable(var));

        let DataVariable(name, _num) = var;

        let first = matches.next().ok_or_else(|| DynamicErr::UnboundSymbol {
            symbol: name.to_string(),
        })?;

        if let Some(second) = matches.next() {
            let DataName(fst) = &first.name;
            let DataName(snd) = &second.name;
            return Err(DynamicErr::AmbiguousSymbol {
                symbol: name.to_string(),
                possibility1: fst.to_string(),
                possibility2: snd.to_string(),
            });
        }

        Ok(first)
    }

    fn data_decl_from_abbr_variant<'grammar>(
        &'grammar self,
        variant: &DataVariant,
    ) -> DynamicRes<(&'grammar DataDecl, &'grammar DataVariant)> {
        let variant_name = variant.as_ref();

        // Search through all data declarations for variants that `val` is
        // an abbreviation of. Collect all those variants.
        // TODO: use the "type signature" of the rule to narrow this search.
        let mut canonicalizations = self.grammar.data_decls.iter().flat_map(|decl| {
            decl.variants
                .iter()
                .filter_map(move |(other_variant, _reprs)| {
                    let DataVariant(other_name) = other_variant;
                    if abbreviates(variant_name, other_name.as_str()) {
                        Some((decl, other_variant))
                    } else {
                        None
                    }
                })
        });

        // Take the first one, and if there are none, that's an unbound
        // symbol error.
        let first = canonicalizations
            .next()
            .ok_or_else(|| DynamicErr::UnboundSymbol {
                symbol: variant_name.to_string(),
            })?;

        // If there's more than one possibility, that's an ambiguity.
        if let Some(second) = canonicalizations.next() {
            let (decl1, DataVariant(possibility1)) = first;
            let (decl2, DataVariant(possibility2)) = second;
            return Err(DynamicErr::AmbiguousSymbol {
                symbol: variant_name.to_string(),
                possibility1: format!("{}::{}", decl1.name.as_ref(), possibility1),
                possibility2: format!("{}::{}", decl2.name.as_ref(), possibility2),
            });
        }

        Ok(first)
    }

    /// Given a variable name, if the current state already has a binding for
    /// that variable, returns the `DataVariant` bound to the variable. If no
    /// binding yet exists, this fn randomly selects a `DataVariant` from the
    /// `DataDecl`'s listed variants, creates a new binding in the state, and
    /// returns the randomly selected `DataVariant`.
    fn value_of_variable<'st>(
        &self,
        var: &DataVariable,
        state: &'st mut State,
    ) -> DynamicRes<&'st mut DataVariant> {
        Ok(state.entry(var.clone()).or_insert({
            let mut iter = self
                .grammar
                .data_decls
                .iter()
                .filter(|decl| decl.name.matches_variable(var));
            let res = iter
                .next()
                .expect(&format!("a rule matching {:?} exists", var));
            let more = iter.next();
            if let Some(more_name) = more {
                return Err(DynamicErr::AmbiguousSymbol {
                    symbol: var.to_string(),
                    possibility1: res.name.to_string(),
                    possibility2: more_name.name.to_string(),
                });
            }
            res.variants
                .keys()
                .choose(&mut *self.rng.borrow_mut())
                .expect("no data decl has 0 variants")
                .clone()
        }))
    }

    fn stringify_data_variant<'gen, 'buf>(
        &'gen self,
        variant: DataVariant,
        state: &mut State,
    ) -> DynamicRes<Vector<OutToken>>
    where
        'buf: 'gen,
    {
        // Collect all `DataDecl`s that contain the variant.
        let decl = self
            .grammar
            .data_decls
            .iter()
            .filter(|decl| decl.variants.contains_key(&variant))
            .next()
            .ok_or_else(|| DynamicErr::UnboundSymbol {
                symbol: variant.to_string(),
            })?;

        // Get the `DataDecl`s stringification alternatives for `variant`.
        let alternatives = decl
            .variants
            .get(&variant)
            .unwrap() // Can't panic because of previous `filter`.
            .clone();

        // Choose one of the alternatives at random.
        let stringification = alternatives
            .iter()
            .choose(&mut *self.rng.borrow_mut())
            .ok_or_else(|| DynamicErr::NoDataVariantStringification {
                symbol: variant.to_string(),
            })?;

        // Generate a sentence based on that sentential form.
        self.generate_non_unique_from_sentence(stringification.clone(), state)
    }

    fn generate_non_unique_from_start<'gen>(
        &'gen self,
        start: RuleName,
        state: &mut State,
    ) -> DynamicRes<Vector<OutToken>> {
        let start_call = Token::RuleRef(RuleRef {
            rule: start,
            vars: vec![],
        });
        let start_sentence = vector![start_call];
        self.generate_non_unique_from_sentence(start_sentence, state)
    }

    fn generate_non_unique_from_sentence<'gen, 'buf>(
        &'gen self,
        sentence: Vector<Token>,
        state: &mut State,
    ) -> DynamicRes<Vector<OutToken>> {
        let mut new_sentence: Vector<OutToken> = Default::default();

        // For each token, if it's a variable, it needs to be replaced.
        for token in sentence {
            match token {
                Token::StrLit(sym) => new_sentence.push_back(sym.into()),
                Token::Plus => new_sentence.push_back(OutToken::Plus),
                Token::DataVariant(variant) => {
                    let to_append = self.stringify_data_variant(variant, state)?;
                    new_sentence.append(to_append);
                }
                Token::DataVariable(ref variable) => {
                    let variant = self.value_of_variable(variable, state)?.clone();
                    let to_append = self.stringify_data_variant(variant, state)?;
                    new_sentence.append(to_append);
                }
                Token::RuleRef(RuleRef { ref rule, ref vars }) => {
                    // Ensure each of `vars` has a binding.
                    let mut arguments = vec![];
                    for arg in vars {
                        match arg {
                            // This is a case like `they.Number` where a variable is being passed in.
                            Argument::Variable(var) => {
                                let thing = self.value_of_variable(var, state)?.clone();
                                arguments.push(thing);
                            }
                            // This is a case like `they.singular` where a data-variant is being passed in.
                            // We need to canonicalize this name in case it is an abbreviation.
                            Argument::Variant(variant) => {
                                let (_decl, variant) = self.data_decl_from_abbr_variant(variant)?;
                                arguments.push((*variant).clone())
                            }
                        }
                    }

                    // Search the grammar's rules via `self.choose_rule`.
                    // If no rule cases are viable, panic.
                    let case = self.choose_rule(rule, &arguments, state)?;
                    let next_sentence = case
                        .alternatives
                        .iter()
                        .choose(&mut *self.rng.borrow_mut())
                        .expect(
                            "Invariant violated by parser: should not be able to have an empty set \
                            of sentential form alternatives!",
                        );
                    let tokens_to_add =
                        self.generate_non_unique_from_sentence(next_sentence.clone(), state)?;
                    new_sentence.append(tokens_to_add);
                }
            }
        }

        Ok(new_sentence)
    }

    /// Takes in a iterator of either `Sym`s or `Plus`es. Before every `Sym`,
    /// insert a space character. If there is a `Plus` before a `Sym`, don't
    /// insert the space. If it's at the beginning of the iterator, don't insert
    /// a space.
    fn join_symbols<'it>(&self, syms: impl Iterator<Item = &'it OutToken> + 'it) -> String {
        let (lower_bound, _upper) = syms.size_hint();
        let mut text = String::with_capacity(lower_bound);
        let mut add_joining_space = false;

        for sym in syms {
            match sym {
                OutToken::Sym(sym) => {
                    if add_joining_space {
                        text.push(' ');
                    }
                    text.push_str(sym.as_str());
                    add_joining_space = true;
                }
                OutToken::Plus => add_joining_space = false,
            }
        }
        text
    }

    pub fn generate<'gen, 'buf>(&'gen mut self) -> DynamicRes<String>
    where
        'buf: 'gen,
    {
        let start = RuleName(IStr::new("start"));
        let mut trials = 0;
        loop {
            let mut state = State::new();
            let sentence = self.generate_non_unique_from_start(start.clone(), &mut state)?;
            if !self.seen_sentences.contains(&sentence) {
                self.seen_sentences.insert(sentence.clone());
                let text = self.join_symbols(sentence.iter());
                break Ok(text);
            } else if trials >= self.max_trials {
                break Err(DynamicErr::MaxTrialsExceeded {
                    trials: self.max_trials,
                });
            } else {
                trials += 1;
            }
        }
    }
}
