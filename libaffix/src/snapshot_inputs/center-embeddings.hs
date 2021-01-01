-- # Generating Center Embeddings
-- [This wikipedia article](https://en.wikipedia.org/wiki/Center_embedding) is about a kind of sentence that is REALLY hard to understand. Click the "Generate" button in the top right to generate sentences!

rule start = center-embedding

rule center-embedding = subject verb-phrase + "."

rule verb-phrase
  = "slept"
  | "killed" subject
  | "longed for the sea"
  | "was murdered on the high seas"
  | "visited" subject

rule subject
  = article[Subj] @Subj
  | article[Subj] @Subj that subject intermediate-verb

data Subj
  = dog ("dog")
  | cat ("cat")
  | hamster ("hamster")
  | porcupine ("porcupine")
  | ostrich ("ostrich")
  | giraffe ("giraffe")
  | sea-turtle ("sea turtle")
  | aardvark ("aardvark")
  | porpoise ("porpoise")
  | dash-hound ("dash hound")
  | pterodactly ("pterodactyl")

rule article[Subj] =
  [ostrich] -> "an" | "the"
  [aardvark] -> "an" | "the"
  [?] -> "a" | "the"
  
rule that = "that" | "who" | ""
  
rule intermediate-verb
  = "loved"
  | "spat upon"
  | "saw"
  | "intended to kill"
  | "arrested"
  | "plucked"
  | "avoided"
  | "was attracted to"
  | "had observed carefully"
  | "annoyed"
  | "consumed"
  | "minimized"
  | "treated harshly in the workplace"
  | "patronized"
  | "complemented"
  | "insulted"
