use crate::fault::{DynamicErr, DynamicRes};
use crate::parser::syntax::{
    abbreviates, Abbr, Argument, Case, DataName, DataVariable, DataVariant, Grammar, Guard,
    Pattern, RuleDecl, RuleName, RuleRef, Token,
};
use im::{vector, Vector};
use internship::IStr;
use rand::seq::IteratorRandom;
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashSet},
};

type State = BTreeMap<DataVariable, DataVariant>;

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

pub struct Generator<Rand: rand::Rng = rand::rngs::ThreadRng> {
    grammar: Grammar,
    rng: RefCell<Rand>,
    seen_sentences: HashSet<im::Vector<OutToken>>,
    pub max_trials: usize,
}

impl Generator<rand::rngs::StdRng> {
    pub fn new_seeded(grammar: Grammar, seed: u64) -> Self {
        Self {
            grammar,
            rng: RefCell::new(rand::SeedableRng::seed_from_u64(seed)),
            seen_sentences: HashSet::new(),
            max_trials: DEFAULT_MAX_TRIALS,
        }
    }
}

impl Generator {
    pub fn new(grammar: Grammar) -> Self {
        Self {
            grammar,
            rng: RefCell::new(rand::thread_rng()),
            seen_sentences: HashSet::new(),
            max_trials: DEFAULT_MAX_TRIALS,
        }
    }
}

impl<Rand> Generator<Rand>
where
    Rand: rand::Rng,
{
    fn choose_case<'gen, 'st>(
        &'gen self,
        rule_decl: &RuleDecl,
        arguments: &Vec<DataVariant>,
        state: &'st mut State,
    ) -> DynamicRes<Case> {
        // Check each case one after another until an allowable case is found.
        for case in &rule_decl.cases {
            let param_typs = &rule_decl.signature.parameter_types;
            let reqs = &case.guard;
            if let Some(new_state) =
                self.allowable(&rule_decl.signature.name, &param_typs, reqs, arguments)?
            {
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
            &rule_decl.signature.name,
            arguments,
        ))
    }

    fn allowable<'st>(
        &self,
        rule_name: &RuleName,
        _types: &Vec<Abbr<DataName>>,
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
                    if !abbreviates(patt_variant, arg) {
                        return Ok(None);
                    }
                }
                Pattern::Variable(var) => {
                    // Lookup the `DataDecl` that the variable refers to.
                    let variable_decl = self.grammar.data_decl_from_abbr_variable(var)?;

                    // If that `DataDecl` doesn't contain the arg, there's a problem.
                    if !variable_decl.variants.contains_key(arg) {
                        // `arg` might be an abbreviation since it's not EXACTLY listed in the
                        // `DataDecl`'s variants map.
                        let arg = Abbr::new(arg.clone());

                        // Look up the `DataDecl` that DOES contain `arg`.
                        let (variant_decl, _variant) =
                            self.grammar.data_decl_from_abbr_variant(&arg)?;

                        return Err(DynamicErr::PatternMatchTypeError {
                            pattern_type: variable_decl.name.to_string(),
                            argument_type: variant_decl.name.to_string(),
                            pattern_variable: var.to_string(),
                        });
                    }

                    // Set the value of `var` to `arg` in the soon-to-be-updated state.
                    let overwritten = state.insert(var.clone(), arg.clone());
                    if overwritten.is_some() {
                        panic!("Shouldn't be overwriting variable bindings here.");
                    }
                }
            }
        }

        Ok(Some(state))
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
        start: Abbr<RuleName>,
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
                                let (_decl, variant) =
                                    self.grammar.data_decl_from_abbr_variant(variant)?;
                                arguments.push((*variant).clone())
                            }
                        }
                    }

                    // Search the grammar's rules via `self.choose_rule`.
                    // If no rule cases are viable, panic.
                    let (rule_decl, rule_name) =
                        self.grammar.rule_decl_from_abbr_rule_name(rule)?;
                    let case = self.choose_case(rule_decl, &arguments, state)?;
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
        let start = Abbr::new(RuleName(IStr::new("start")));
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
