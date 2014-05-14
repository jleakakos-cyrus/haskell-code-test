{-# LANGUAGE OverloadedStrings #-}
module Parser where 

import Person
import Data.List.Split (splitOn)
import qualified Data.Text as T

pipeDelim :: T.Text
pipeDelim = "|"

makePipePerson :: T.Text -> Person
makePipePerson line = Person fn ln (makeGender g) c (makeDay dateDashFormat dob)
 where (ln:fn:_:g:c:dob:[]) = splitLine pipeDelim line

commaDelim :: T.Text
commaDelim = ","

makeCommaPerson :: T.Text -> Person
makeCommaPerson line = Person fn ln (makeGender g) c (makeDay dateSlashFormat dob)
 where (ln:fn:g:c:dob:[]) = splitLine commaDelim line

spaceDelim :: T.Text
spaceDelim = " "

makeSpacePerson :: T.Text -> Person
makeSpacePerson line = Person fn ln (makeGender g) c (makeDay dateDashFormat dob)
 where (ln:fn:_:g:dob:c:[]) = splitLine spaceDelim line

splitLine :: T.Text -> T.Text -> [T.Text]
splitLine delim line = map T.strip $ T.splitOn delim line
