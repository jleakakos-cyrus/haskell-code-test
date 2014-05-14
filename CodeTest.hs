module CodeTest where

import Person
import Parser
import Data.Text (pack)

main = do
  pipeContent <- readFile "pipe.txt"
  let pipeLines = lines pipeContent
  let pipePeople = map (makePipePerson . pack) pipeLines

  commaContent <- readFile "comma.txt"
  let commaLines = lines commaContent
  let commaPeople = map (makeCommaPerson . pack) commaLines
  
  spaceContent <- readFile "space.txt"
  let spaceLines = lines spaceContent
  let spacePeople = map (makeSpacePerson . pack) spaceLines

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
