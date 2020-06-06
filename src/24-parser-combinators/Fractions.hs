{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction =
  decimal >>=
    \numerator ->
      char '/' >>
        decimal >>=
          \denominator ->
            case denominator of
              0 -> fail "Ratio must have nonzero denominator."
              _ -> return (numerator % denominator)

parseDecimal :: Parser Rational
parseDecimal = do
  whole <- decimal
  char '.'
  fractional <- decimal

  let fractionalNum = fromInteger fractional
      fractionalLength   = ceiling (logBase 10 fractionalNum)
      fractionalPower = 1 / (10 ^ fractionalLength)
      wholeNum = fromInteger whole

  return . toRational $
    wholeNum + fractionalNum * fractionalPower

parseDecimalOrFraction :: Parser Rational
parseDecimalOrFraction =
  try parseFraction <|> parseDecimal

main :: IO ()
main = do
  let parseFraction' =
        parseString parseFraction mempty
  
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction
