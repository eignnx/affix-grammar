# affix-grammar

Based on [this paper](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.53.5264&rep=rep1&type=pdf) about [Affix Grammars](https://en.m.wikipedia.org/wiki/Affix_grammar). This project is heavily inspired by [Dr. Kate Compton](https://github.com/galaxykate)'s [Tracery Project](https://github.com/galaxykate/tracery).

Generates sentences based on a grammar, but does not parse sentences.

## Example Grammar

**NOTE:** syntax is likely to change, this may or may not be up-to-date.

```haskell
data Number = singular | plural
data Gender = masculine | feminine | nonbinary

rule start =
    they.N1.G1 "said to" themself.N1.G1 "'" + they.N2.G2 are.N2.G2 "mean.'"

rule they.Number.Gender =
    .singular {
        .masculine -> "he"
        .feminine -> "she"
        .nonbinary -> "they"
    }
    .plural {
        .masculine -> "they"
        .feminine -> "they"
        .nonbinary -> "they"
    }

rule themself.Number.Gender =
    .singular {
        .masculine -> "himself"
        .feminine -> "herself"
        .nonbinary -> "themself"
    }
    .plural {
        .masculine -> "themselves"
        .feminine -> "themselves"
        .nonbinary -> "themselves"
    }

rule are.Number.Gender =
    .singular.masculine -> "is"
    .singular.feminine -> "is"
    .singular.nonbinary -> "are"
    .plural.masculine -> "are"
    .plural.feminine -> "are"
    .plural.nonbinary -> "are"

```

## Generate Sentences

Use the CLI tool to generate sentences. Ensure you have [`cargo`](https://doc.rust-lang.org/cargo/getting-started/installation.html) installed beforehand.

```shell
$ cargo run path/to/grammar-file -i
```

See the [`test_grammars`](https://github.com/eignnx/affix-grammar/tree/master/test_grammars) directory for example grammar files (though many are not written with current syntax).
