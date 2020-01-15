ls test_grammars | while read file; do
    echo $file
    cat test_grammars/$file | cargo run
done