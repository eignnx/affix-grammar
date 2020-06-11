data GrammaticalGender = neutral | nonbinary | feminine ("f") | masculine ("m")

rule start = they.Gender "walked to the store."

rule they.Gender =
    .nb -> "they"
    .ntrl -> "it"
    .Gender1 -> "they" "(" + @Gender1 + ")"
