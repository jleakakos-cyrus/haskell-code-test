{-# LANGUAGE OverloadedStrings #-}
module APParser where
import Prelude hiding (takeWhile)

import Person

import Data.Char (isSpace)
import Data.Attoparsec.Text
import Data.Text (strip, Text)
import Control.Applicative
import qualified Data.Text.IO as TIO

main = do
  commaContent <- TIO.readFile "data/comma.txt"
  let Right commaPeople = parseOnly (parsePeople ',') commaContent
  pipeContent <- TIO.readFile "data/pipe.txt"
  let Right pipePeople = parseOnly (parsePeople '|') pipeContent
  spaceContent <- TIO.readFile "data/space.txt"
  let Right spacePeople = parseOnly (parsePeople ' ') spaceContent
  let people = pipePeople ++ commaPeople ++ spacePeople
  putStrLn "Output 1:"
  mapM_ print $ sortByGenderAndLastNameAscending people
  putStrLn ""
  putStrLn "Output 2:"
  mapM_ print $ sortByBirthDateAscending people
  putStrLn ""
  putStrLn "Output 3:"
  mapM_ print $ sortByLastNameDescending people
  putStrLn ""

parsePeople :: Char -> Parser [Person]
parsePeople delim 
  | delim == ',' = many' $ (parseCommaPerson) <* (endOfLine <|> endOfInput)
  | delim == '|' = many' $ (parsePipePerson) <* (endOfLine <|> endOfInput)
  | delim == ' ' = many' $ (parseSpacePerson) <* (endOfLine <|> endOfInput)

parseSpacePerson :: Parser Person
parseSpacePerson =
  let isContent c = (not $ isSpace c)
  in do
    skipSpace
    ln <- takeWhile isContent
    skipDelim ' '
    skipSpace
    fn <- takeWhile isContent
    skipDelim ' '
    skipSpace
    mi <- takeWhile isContent
    skipDelim ' '
    skipSpace
    g <- takeWhile isContent
    skipDelim ' '
    skipSpace
    dob <- takeWhile isContent
    skipDelim ' '
    skipSpace
    fc <- takeWhile isContent
    return $ Person fn ln (makeGender g) fc (makeDay dateDashFormat dob)

parsePipePerson :: Parser Person
parsePipePerson =
  let isContent c = (not $ isSpace c) && (c /= '|' )
  in do
    skipSpace
    ln <- takeWhile isContent
    skipSpace
    skipDelim '|'
    skipSpace
    fn <- takeWhile isContent
    skipSpace
    skipDelim '|'
    skipSpace
    mi <- takeWhile isContent
    skipSpace
    skipDelim '|'
    skipSpace
    g <- takeWhile isContent
    skipSpace
    skipDelim '|'
    skipSpace
    fc <- takeWhile isContent
    skipSpace
    skipDelim '|'
    skipSpace
    dob <- takeWhile isContent
    return $ Person fn ln (makeGender g) fc (makeDay dateDashFormat dob)

parseCommaPerson :: Parser Person
parseCommaPerson =
  let isContent c = (not $ isSpace c) && (c /= ',' )
  in do
    skipSpace
    ln <- takeWhile isContent
    skipSpace
    skipDelim ','
    skipSpace
    fn <- takeWhile isContent
    skipSpace
    skipDelim ','
    skipSpace
    g <- takeWhile isContent
    skipSpace
    skipDelim ','
    skipSpace
    fc <- takeWhile isContent
    skipSpace
    skipDelim ','
    skipSpace
    dob <- takeWhile isContent
    return $ Person fn ln (makeGender g) fc (makeDay dateSlashFormat dob)

skipDelim :: Char -> Parser ()
skipDelim delim = skip $ (== delim)


