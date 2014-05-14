{-# LANGUAGE OverloadedStrings #-}
module Parser where 

import Person (makeGender, makeDay, dateDashFormat, dateSlashFormat, Person(..))
import Data.List.Split (splitOn)
import qualified Data.Text as T

makePipePerson :: T.Text -> Person
makePipePerson line = Person fn ln (makeGender g) c (makeDay dateDashFormat dob)
 where (ln:fn:_:g:c:dob:[]) = splitLine "|" line

makeCommaPerson :: T.Text -> Person
makeCommaPerson line = Person fn ln (makeGender g) c (makeDay dateSlashFormat dob)
 where (ln:fn:g:c:dob:[]) = splitLine "," line

makeSpacePerson :: T.Text -> Person
makeSpacePerson line = Person fn ln (makeGender g) c (makeDay dateDashFormat dob)
 where (ln:fn:_:g:dob:c:[]) = splitLine " " line

detectDelimAndSplitSlint :: T.Text -> [T.Text]
detectDelimAndSplitSlint = undefined

splitLine :: T.Text -> T.Text -> [T.Text]
splitLine delim line = map T.strip $ T.splitOn delim line
