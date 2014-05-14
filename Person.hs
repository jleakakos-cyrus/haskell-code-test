{-# LANGUAGE OverloadedStrings #-}
module Person 
  (Person(..),
   Gender(..),
   dateDashFormat,
   dateSlashFormat,
   makeGender,
   makeDay,
   sortByLastNameDescending,
   sortByBirthDateAscending,
   sortByGenderAndLastNameAscending)
where

import Data.List (sortBy)
import Data.Time (utctDay, Day, UTCTime)
import Data.Time.Format (readTime)
import System.Locale (defaultTimeLocale)
import qualified Data.Text as T

data Gender = Male | Female | Other deriving (Eq, Show)

data Person = Person { firstName :: T.Text, lastName :: T.Text, gender :: Gender, favoriteColor :: T.Text, dateOfBirth :: Day } deriving (Eq)

instance Show Person where
  show (Person fn ln g c dob) = show $ T.intercalate " " [ln, fn, (T.pack . show) g, (T.pack . show) dob, c]

-- SORTING
sortByGenderAndLastNameAscending :: [Person] -> [Person]
sortByGenderAndLastNameAscending = sortBy genderAndLastNameAscending

sortByBirthDateAscending :: [Person] -> [Person]
sortByBirthDateAscending = sortBy birthDateAscending

sortByLastNameDescending :: [Person] -> [Person]
sortByLastNameDescending = sortBy lastNameDescending

genderAndLastNameAscending :: Person -> Person -> Ordering
genderAndLastNameAscending (Person _ ln1 g1 _ _) (Person _ ln2 g2 _ _)
  | g1 == Female && g2 == Male = LT
  | g1 == Male && g2 == Female = GT
  | otherwise = compare ln1 ln2

birthDateAscending :: Person -> Person -> Ordering
birthDateAscending (Person _ _ _ _ dob1) (Person _ _ _ _ dob2) = compare dob1 dob2

lastNameDescending :: Person -> Person -> Ordering
lastNameDescending (Person _ ln1 _ _ _) (Person _ ln2 _ _ _) = compare ln2 ln1

makeGender :: T.Text -> Gender
makeGender g
  | g == "Female" || g == "F" = Female
  | g == "Male" || g == "M" = Male
  | otherwise = Other

newtype DateFormat = DateFormat T.Text deriving (Show)

dateSlashFormat :: DateFormat
dateSlashFormat = DateFormat "%-m/%-d/%Y"

dateDashFormat :: DateFormat
dateDashFormat = DateFormat "%-m-%-d-%Y"

makeDay :: DateFormat -> T.Text -> Day
makeDay (DateFormat format) date = utctDay (readTime defaultTimeLocale formatString dateString :: UTCTime)
  where dateString = show date
        formatString = show format
