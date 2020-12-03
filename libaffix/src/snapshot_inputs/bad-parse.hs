data Abc = a | b | c

rule start = asdf.Abc.Abc

rule asdf.Abc.Abc =
    .a {
        .a -> "asdf" | "qwer"
        .b -> "asdf"
        .c -> "asdf"
    }
    .b {
        .? -> "uh-oh"
    -- }
    .c.? -> "poiu"
