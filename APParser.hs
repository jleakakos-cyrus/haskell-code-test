{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (takeWhile)
import Person

import Data.Char (isAlpha, isSpace)
import Data.Attoparsec.Text
import Data.Text (strip, Text)
import Control.Applicative
import qualified Data.Text.IO as TIO

main = do
  commaContent <- TIO.readFile "data/comma.txt"
  let Right commaPeople = parseOnly parseCommaPeople commaContent
  pipeContent <- TIO.readFile "data/pipe.txt"
  let Right pipePeople = parseOnly parsePipePeople pipeContent
  spaceContent <- TIO.readFile "data/space.txt"
  let Right spacePeople = parseOnly parseSpacePeople spaceContent
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

parseCommaPeople :: Parser [Person]
parseCommaPeople = sepBy parseCommaPerson endOfLine

parsePipePeople :: Parser [Person]
parsePipePeople = sepBy parsePipePerson endOfLine

parseSpacePeople :: Parser [Person]
parseSpacePeople = sepBy parseSpacePerson endOfLine

parseSpacePerson :: Parser Person
parseSpacePerson =
   do
    skipSpace
    ln <- takeTill isSpace
    skipSpace
    fn <- takeTill isSpace
    skipSpace
    mi <- takeTill isSpace
    skipSpace
    g <- takeTill isSpace
    skipSpace
    dob <- takeTill isSpace
    skipSpace
    fc <- takeTill isSpace
    return $ Person fn ln (makeGender g) fc (makeDay dateDashFormat dob)

skipDelim :: Char -> Parser ()
skipDelim delim = skipSpace >> char delim >> skipSpace

parsePipePerson :: Parser Person
parsePipePerson =
  let isContent c = (not $ isSpace c) && (c /= '|' )
  in do
    skipSpace
    ln <- takeWhile isContent
    skipDelim '|'
    fn <- takeWhile isContent
    skipDelim '|'
    mi <- takeWhile isContent
    skipDelim '|'
    g <- takeWhile isContent
    skipDelim '|'
    fc <- takeWhile isContent
    skipDelim '|'
    dob <- takeWhile isContent
    return $ Person fn ln (makeGender g) fc (makeDay dateDashFormat dob)

parseCommaPerson :: Parser Person
parseCommaPerson =
  let isContent c = (not $ isSpace c) && (c /= ',' )
  in do
    skipSpace
    ln <- takeWhile isContent
    skipDelim ','
    fn <- takeWhile isContent
    skipDelim ','
    g <- takeWhile isContent
    skipDelim ','
    fc <- takeWhile isContent
    skipDelim ','
    dob <- takeWhile isContent
    return $ Person fn ln (makeGender g) fc (makeDay dateSlashFormat dob)
