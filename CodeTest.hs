{-# LANGUAGE OverloadedStrings #-}
module Main where

import Person
import Person (makeGender, makeDay, dateDashFormat, dateSlashFormat, Person(..))
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  pipeContent <- TIO.readFile "data/pipe.txt"
  let pipeLines = T.lines pipeContent
  let pipePeople = map makePipePerson pipeLines
  commaContent <- TIO.readFile "data/comma.txt"
  let commaLines = T.lines commaContent
  let commaPeople = map makeCommaPerson commaLines
  spaceContent <- TIO.readFile "data/space.txt"
  let spaceLines = T.lines spaceContent
  let spacePeople = map makeSpacePerson spaceLines
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

makePipePerson :: T.Text -> Person
makePipePerson line = Person fn ln (makeGender g) c (makeDay dateDashFormat dob)
 where (ln:fn:_:g:c:dob:[]) = splitLine "|" line

makeCommaPerson :: T.Text -> Person
makeCommaPerson line = Person fn ln (makeGender g) c (makeDay dateSlashFormat dob)
 where (ln:fn:g:c:dob:[]) = splitLine "," line

makeSpacePerson :: T.Text -> Person
makeSpacePerson line = Person fn ln (makeGender g) c (makeDay dateDashFormat dob)
 where (ln:fn:_:g:dob:c:[]) = splitLine " " line

splitLine :: T.Text -> T.Text -> [T.Text]
splitLine delim line = map T.strip $ T.splitOn delim line

-- TODO Autodetect the delimiter and make a person 
makePerson :: T.Text -> Maybe Person
makePerson = undefined

