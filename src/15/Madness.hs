-- Exercises, Page 600

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' exclamation adverb noun adjective =
  "\"" <> exclamation <> "!\" he said "
       <> adverb      <> " as he jumped into his car "
       <> noun        <> " and drove off with his "
       <> adjective   <> " wife." 

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' exclamation adverb noun adjective = mconcat
  [
    "\""
  , exclamation
  , "!\" he said "
  , adverb
  , " as he jumped into his car "
  , noun
  , " and drove off with his "
  , adjective
  , " wife."
  ]