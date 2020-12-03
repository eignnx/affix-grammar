data EnglishGrammaticalGender = neutral | nonbinary | feminine | masculine
data EnglishGrammaticalPerson = 1st | 2nd | 3rd
data EnglishGrammaticalNumber = singular | plural

rule start = they.Number.Person.Gender "went to the store."

rule they.Number.Person.Gender =
  .singular {
    .1st.? -> "I"
    .2nd.? -> "you"
    .3rd {
      .neutral -> "it"
      .nonbinary -> "they"
      .feminine -> "she"
    --   .masculine -> "he"
    }
  }
  .plural {
    .1st.? -> "we"
    .2nd.masculine -> they.plural.2nd.neutral | "you guys"
    .2nd.? -> "you" | "y'all"
    .3rd.? -> "they"
  }
