{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec

import Text.RawString.QQ

import Text.Trifecta

headerEx :: ByteString
headerEx = "[blah]"

newtype Header =
  Header String
  deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair parser =
  char '[' *> parser <* char ']'

parseHeader :: Parser Header
parseHeader =
  parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _    <- char '='
  val  <- some (noneOf "\n")
  skipEOL
  return (name, val)

-- Skip end of line and whitespace beyond.

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

commentEx :: ByteString
commentEx =
  "; Last modified on the April 1, 2001 by John Doe."

commentEx' :: ByteString
commentEx' =
  "; Don't trip \n over ; all of these \n; odd characters ;\n."

skipComments :: Parser ()
skipComments =
  skipMany $ do
    _ <- char ';' <|> char '#'
    skipMany (noneOf "\n")
    skipEOL

sectionEx :: ByteString
sectionEx =
  "; Ignore me.\n[states]\nchris=texas"

sectionEx' :: ByteString
sectionEx' = [r|
  ; Comment.
  [section]
  host=wikipedia.org
  alias=claw
  
  [whatisit]
  red=intoothandclaw
|]

data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments

  header <- parseHeader
  skipEOL

  assignments <- some parseAssignment
  return $
    Section header (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section header assignments) headersToAssignments =
  M.insert header assignments headersToAssignments

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection

  let mapOfSections =
        foldr rollup M.empty sections

  return (Config mapOfSections)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "Assignment parsing." $
    it "Can parse simple assignments." $ do
      let m = parseByteString parseAssignment mempty assignmentEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ("woot", "1")

-- ...
