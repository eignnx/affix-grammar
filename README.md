# affix-grammar

Based on [this paper](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.53.5264&rep=rep1&type=pdf) about [Affix Grammars](https://en.m.wikipedia.org/wiki/Affix_grammar). This project is heavily inspired by [Dr. Kate Compton](https://github.com/galaxykate)'s [Tracery Project](https://github.com/galaxykate/tracery).

Generates sentences based on a grammar, but does not parse sentences.

## Project Structure

- the `libaffix` folder is a crate that defines all of the internal logic of lexer, parser, grammar datatypes, and generator.
- the `affix-gramar-js` is a `wasm-pack` project that exports a webassembly interface for `libaffix`.
- the main folder (`affix-grammar`) is a command line app which can be used to generate sentences based on grammar files from the command line.

## Examples

### Example Grammar 1

The top-level of the grammar is the `start` rule, and expansion will begin from `start`.

When a rule is referenced (like `odd-or-even-pair` or `number[Bit1][Bit2]`), if it has parameters, they must be passed in at time of call.

Variables can be suffixed with a number to make them unique. In this example, `Bit2` in the first call to the `number` will always to have the same value as `Bit2` in the second call to the `number` rule.

The effect of this example is to produce pairs of numbers that are either:

- both even,
- or both odd.

```haskell
data Bit = 0 | 1

rule start = odd-or-even-pair

rule odd-or-even-pair = "{" number[Bit1][Bit2] "} {" number[Bit3][Bit2] "}"

rule number[Bit][Bit] =
    [0][0] -> "00, zero (even)"
    [0][1] -> "01, one (odd)"
    [1][0] -> "10, two (even)"
    [1][1] -> "11, three (odd)"
```

The following sentences will be produced (not necessarily in this order):

```
"{ 10, two (even) } { 10, two (even) }"
"{ 00, zero (even) } { 00, zero (even) }"
"{ 11, three (odd) } { 11, three (odd) }"
"{ 00, zero (even) } { 10, two (even) }"
"{ 10, one (odd) } { 01, one (odd) }"
"{ 11, three (odd) } { 01, one (odd) }"
"{ 10, two (even) } { 00, zero (even) }"
"{ 01, one (odd) } { 11, three (odd) }"
```

### Example Grammar 2

```haskell
data Number = singular | plural
data Person = 1st | 2nd | 3rd
data Gender = masculine | feminine | nonbinary | neutral

-- The expression `they[N][P][G]` is a reference to a rule (kinda like a function
-- call).
-- The arguments N, P, and G are variables that are bound to values implicitly,
-- for instance, N might be bound to the Number variant `plural`.
-- N is an abbreviation for Number, P is some Person, G is Gender.
-- The full names of the data types can be spelled out, but for brevity, the can
-- be abbreviated as long as there is no ambiguity.
rule start = they[N][P][G] look[N][P][G] "at" themself[N][P][G] "in the mirror."

-- This is a rule that knows how to conjugate the present-tense verb "to look".
-- The `?` pattern matches any value.
rule look[Number][Person][Gender] =
    [singular][3rd][nonbinary] -> "look"
    [singular][3rd][?] -> "looks"
    [?][?][?] -> "look"

rule they[Number][Person][Gender] =
    [singular] {
        [1st][?] -> "I"
        [2nd][?] -> "you"
        [3rd] {
            [masculine] -> "he"
            [feminine] -> "she"
            [nonbinary] -> "they"
            [neutral] -> "it"
        }
    }
    [plural] {
        [1st][?] -> "we"
        [2nd][?] -> "y'all" | "you" -- Note: you can separate alternative values by a `|`.
        [3rd][?] -> "they"
    }

rule themself[Number][Person][Gender] =
    [singular] {
        [1st][?] -> "myself"
        [2nd][?] -> "yourself"
        [3rd] {
            [masculine] -> "himself"
            [feminine] -> "herself"
            [nonbinary] -> "themself"
            [neutral] -> "itself"
        }
    }
    [plural] {
        [1st][?] -> "ourselves"
        [2nd][?] -> "yourselves"
        [3rd][?] -> "themselves"
    }
```

The following sentences will be produced (not necessarily in this order):

```
"we look at ourselves in the mirror."
"y'all look at yourselves in the mirror."
"you look at yourselves in the mirror."
"you look at yourself in the mirror."
"she looks at herself in the mirror."
"I look at myself in the mirror."
"they look at themselves in the mirror."
"they look at themself in the mirror."
"it looks at itself in the mirror."
"he looks at himself in the mirror."
```

## Generate Sentences

Use the CLI tool to generate sentences. Ensure you have [`cargo`](https://doc.rust-lang.org/cargo/getting-started/installation.html) installed beforehand.

```shell
$ cargo run -- path/to/grammar-file -i
```

See the [`libaffix/src/snapshot_inputs`](https://github.com/eignnx/affix-grammar/tree/master/libaffix/src/snapshot_inputs) directory for example grammar files (though note that some are intended to fail).
