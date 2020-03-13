# affix-grammar

Based on [this paper](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.53.5264&rep=rep1&type=pdf) about [Affix Grammars](https://en.m.wikipedia.org/wiki/Affix_grammar). This project is heavily inspired by [Dr. Kate Compton](https://github.com/galaxykate)'s [Tracery Project](https://github.com/galaxykate/tracery).

Generates sentences based on a grammar, but does not parse sentences.

## Example Grammar

```haskell
data Number = singular | plural
data Person = 1st | 2nd | 3rd
data Gender = masculine | feminine | nonbinary | neutral

-- The `start` rule must always be defined. This is the rule that is expanded
-- first.
-- The expression `they.N.P.G` is a reference to a rule (kinda like a function
-- call).
-- The arguments N, P, and G are variables that are bound to values implicitly,
-- for instance, N might be bound to the Number variant `plural`.
-- N is an abbreviation for Number, P is some Person, G is Gender.
-- The full names of the data types can be spelled out, but for brevity, the can
-- be abbreviated as long as there is no ambiguity.
rule start = they.N.P.G look.N.P.G "at" themself.N.P.G "in the mirror."

-- This is a rule that knows how to conjugate the present-tense verb "to look".
rule look.Number.Person.Gender =
    .singular.3rd.nonbinary -> "look"
    .singular.3rd.* -> "looks"
    .*.*.* -> "look"

rule they.Number.Person.Gender =
    .singular {
        .1st.* -> "I"
        .2nd.* -> "you"
        .3rd {
            .masculine -> "he"
            .feminine -> "she"
            .nonbinary -> "they"
            .neutral -> "it"
        }
    }
    .plural {
        .1st.* -> "we"
        .2nd.* -> "y'all" | "you"
        .3rd.* -> "they"
    }

rule themself.Number.Person.Gender =
    .singular {
        .1st.* -> "myself"
        .2nd.* -> "yourself"
        .3rd {
            .masculine -> "himself"
            .feminine -> "herself"
            .nonbinary -> "themself"
            .neutral -> "itself"
        }
    }
    .plural {
        .1st.* -> "ourselves"
        .2nd.* -> "yourselves"
        .3rd.* -> "themselves"
    }

```

## Generate Sentences

Use the CLI tool to generate sentences. Ensure you have [`cargo`](https://doc.rust-lang.org/cargo/getting-started/installation.html) installed beforehand.

```shell
$ cargo run path/to/grammar-file -i
```

See the [`test_grammars`](https://github.com/eignnx/affix-grammar/tree/master/test_grammars) directory for example grammar files (though many are not written with current syntax).
