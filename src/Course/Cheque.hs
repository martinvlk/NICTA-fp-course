{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Bind
import Course.Parser
import Course.MoreParser
import Course.Applicative

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion :: List Chars
illion =
  let preillion :: List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion :: List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit = Zero
           | One
           | Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine deriving (Eq, Enum, Bounded)

showDigit :: Digit -> Chars
showDigit Zero = "zero"
showDigit One = "one"
showDigit Two = "two"
showDigit Three = "three"
showDigit Four = "four"
showDigit Five = "five"
showDigit Six = "six"
showDigit Seven = "seven"
showDigit Eight = "eight"
showDigit Nine = "nine"

instance Show Digit where
  show = hlist . showDigit

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 = D1 Digit
            | D2 Digit Digit
            | D3 Digit Digit Digit deriving (Eq, Show)

-- Possibly convert a character to a digit.
fromChar :: Char -> Optional Digit
fromChar '0' = Full Zero
fromChar '1' = Full One
fromChar '2' = Full Two
fromChar '3' = Full Three
fromChar '4' = Full Four
fromChar '5' = Full Five
fromChar '6' = Full Six
fromChar '7' = Full Seven
fromChar '8' = Full Eight
fromChar '9' = Full Nine
fromChar _ = Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars :: Chars -> Chars
dollars inp = render whl "" "dollar" ++ render (handleCents dec) " and" "cent"
  where (Result _ (whl, dec)) = parse numAsGroups inp
        handleCents Nil = D1 Zero :. Nil
        handleCents (D1 d:._) = D2 d Zero :. Nil
        handleCents (D3 d1 d2 _:._) = D2 d1 d2 :. Nil
        handleCents gs = gs


-- PARSING

numAsGroups :: Parser (List Digit3, List Digit3)
numAsGroups = do gs1 <- groups
                 skip $ is '.'
                 gs2 <- groups2
                 return (checkZero gs1, checkZero gs2)
  where checkZero Nil = D1 Zero :. Nil
        checkZero gs = gs

groups :: Parser (List Digit3)
groups = gg1 ||| gg2 ||| gg3
  where gg1 = do g1 <- group1
                 gs <- list group3
                 endg
                 return $ g1 :. gs
        gg2 = do g2 <- group2
                 gs <- list group3
                 endg
                 return $ g2 :. gs
        gg3 = do gs <- list group3
                 endg
                 return gs
        endg = is '.' ||| eof >>> pure '#'

skip :: Parser a -> Parser (List a)
skip = list

groups2 :: Parser (List Digit3)
groups2 = do gs2 <- list group2'
             gs1 <- list group1'
             return $ if gs2 /= Nil
                      then take 1 gs2
                      else gs1
  where group1' = do skipNonDigits2
                     d1 <- digitTok2
                     return $ D1 d1
        group2' = do skipNonDigits2
                     d1 <- digitTok2
                     d2 <- digitTok2
                     return $ D2 d1 d2
        digitTok2 = do r <- digit
                       skipNonDigits2
                       case fromChar r of
                         Full d -> return d
                         Empty -> unexpectedCharParser r
        skipNonDigits2 = do skip $ noneof "0123456789"
                            return ()

group3 :: Parser Digit3
group3 = do skipNonDigits
            d1 <- digitTok
            d2 <- digitTok
            d3 <- digitTok
            return $ D3 d1 d2 d3

group2 :: Parser Digit3
group2 = do skipNonDigits
            d1 <- digitTok
            d2 <- digitTok
            return $ D2 d1 d2

group1 :: Parser Digit3
group1 = do skipNonDigits
            d1 <- digitTok
            return $ D1 d1

digitTok :: Parser Digit
digitTok = do r <- digit
              skipNonDigits
              case fromChar r of
                Full d -> return d
                Empty -> unexpectedCharParser r

skipNonDigits :: Parser ()
skipNonDigits = do skip $ noneof "0123456789."
                   return ()

-- RENDERING

render :: List Digit3 -> Chars -> Chars -> Chars
render grps pref post = prefix pref ++ rend ++ handlePlural grps' post
  where grps' = zipWith (flip (,)) illion (reverse grps)
        rend = flatMap (++" ") . reverse $ renderGrp <$> eliminateZeroGs grps'

eliminateZeroGs :: List (Digit3, Chars) -> List (Digit3, Chars)
eliminateZeroGs = foldRight elim Nil
  where elim (D3 Zero Zero Zero, _) gs = gs
        elim g gs = g :. gs

handlePlural :: List (Digit3, Chars) -> Chars -> Chars
handlePlural ((D1 One, _):._) s = s
handlePlural ((D2 _ One, _):._) s = s
handlePlural ((D3 _ _ One, _):._) s = s
handlePlural _ s = s ++ "s"

renderGrp :: (Digit3, Chars) -> Chars
renderGrp (D1 d1, lbl) = showDigit d1 ++ label lbl
renderGrp (D2 Zero d2, lbl) = renderGrp (D1 d2, lbl)
renderGrp (D2 One d2, lbl) = renderTeens d2 ++ label lbl
renderGrp (D2 d1 Zero, lbl) = renderTens d1 ++ label lbl
renderGrp (D2 d1 d2, lbl) = (renderTens d1 ++ "-" ++ showDigit d2) ++ label lbl
renderGrp (D3 Zero Zero d3, lbl) = renderGrp (D1 d3, lbl)
renderGrp (D3 Zero d2 d3, lbl) = renderGrp (D2 d2 d3, lbl)
renderGrp (D3 d1 d2 d3, lbl) | (d2 == d3) && d3 == Zero =
                                 showHun d1 ++ label lbl
                             | otherwise =
                                 showHun d1 ++ (" and" `prefix2` renderGrp (D2 d2 d3, lbl))
  where showHun d = showDigit d ++ " hundred"

label :: Chars -> Chars
label "" = ""
label s = " " ++ s

prefix :: Chars -> Chars
prefix "" = ""
prefix s = s ++ " "

prefix2 :: Chars -> Chars -> Chars
prefix2 _ "" = ""
prefix2 pf s = pf ++ " " ++ s

renderTeens :: Digit -> Chars
renderTeens Zero = "ten"
renderTeens One = "eleven"
renderTeens Two = "twelve"
renderTeens Three = "thirteen"
renderTeens Four = "fourteen"
renderTeens Five = "fifteen"
renderTeens Six = "sixteen"
renderTeens Seven = "seventeen"
renderTeens Eight = "eighteen"
renderTeens Nine = "nineteen"

renderTens :: Digit -> Chars
renderTens Two = "twenty"
renderTens Three = "thirty"
renderTens Four = "forty"
renderTens Five = "fifty"
renderTens Six = "sixty"
renderTens Seven = "seventy"
renderTens Eight = "eighty"
renderTens Nine = "ninety"
renderTens n = error $ show ("undefined ten: " ++ show' n)
