ls test_grammars | while read file; do
    echo $file
    cargo run -- test_grammars/$file -n 5
done