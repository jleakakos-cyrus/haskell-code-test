module CodeTest
  (Person,
   Gender,
   sortByGenderAndLastNameAscending,
   sortByBirthDateAscending,
   sortByLastNameDescending)
where

import Data.List (intercalate, sortBy)
import Data.Time (utctDay, Day, UTCTime)
import Data.Time.Format (readTime)
import System.Locale (defaultTimeLocale)

main = do
  putStrLn "Output 1:"
  mapM_ (putStrLn . show) $ sortByGenderAndLastNameAscending ps
  putStrLn ""

  putStrLn "Output 2:"
  mapM_ (putStrLn . show) $ sortByBirthDateAscending ps
  putStrLn ""

  putStrLn "Output 3:"
  mapM_ (putStrLn . show) $ sortByLastNameDescending ps
  putStrLn ""

-- Model
data Gender = Male | Female deriving (Eq, Show)
type Name = String
type FirstName = Name
type LastName = Name
type Color = String
type DOB = Day

data Person = Person { firstName :: Name, lastName :: Name, gender :: Gender, favoriteColor :: Color, dateOfBirth :: DOB } deriving (Eq)

instance Show Person where
  show (Person fn ln g c dob) = intercalate " " [ln, fn, show g, show dob, c]

sortByGenderAndLastNameAscending :: [Person] -> [Person]
sortByGenderAndLastNameAscending = sortBy genderAndLastNameAscending

sortByBirthDateAscending :: [Person] -> [Person]
sortByBirthDateAscending = sortBy birthDateAscending

sortByLastNameDescending :: [Person] -> [Person]
sortByLastNameDescending = sortBy lastNameDescending

-- Sort Orderings
genderAndLastNameAscending :: Person -> Person -> Ordering
genderAndLastNameAscending (Person _ ln1 g1 _ _) (Person _ ln2 g2 _ _)
  | g1 == Female && g2 == Male = LT
  | g1 == Male && g2 == Female = GT
  | otherwise = compare ln1 ln2

birthDateAscending :: Person -> Person -> Ordering
birthDateAscending (Person _ _ _ _ dob1) (Person _ _ _ _ dob2) = compare dob1 dob2

lastNameDescending :: Person -> Person -> Ordering
lastNameDescending (Person _ ln1 _ _ _) (Person _ ln2 _ _ _) = compare ln2 ln1

-- UTILITY STUFF TO HELP WITH GHCI TESTING
makeGender :: String -> Gender
makeGender g
  | g == "Female" = Female
  | g == "Male" = Male

makePersonWithSlashDates :: String -> String -> String -> String -> String -> Person
makePersonWithSlashDates ln fn g dob c = Person fn ln (makeGender g) c day
  where day = utctDay (readTime defaultTimeLocale "%-m/%-d/%Y" dob :: UTCTime)

p1 = makePersonWithSlashDates "Hingis" "Martina" "Female" "4/2/1979" "Green"
p2 = makePersonWithSlashDates "Kelly" "Sue" "Female" "7/12/1959" "Pink"
p3 = makePersonWithSlashDates "Kournikova" "Anna" "Female" "6/3/1975" "Red"
p4 = makePersonWithSlashDates "Seles" "Monica" "Female" "12/2/1973" "Black"
p5 = makePersonWithSlashDates "Abercrombie" "Neil" "Male" "2/13/1943" "Tan"
p6 = makePersonWithSlashDates "Bishop" "Timothy" "Male" "4/23/1967" "Yellow"
p7 = makePersonWithSlashDates "Bonk" "Radek" "Male" "6/3/1975" "Green"
p8 = makePersonWithSlashDates "Bouillon" "Francis" "Male" "6/3/1975" "Blue"
p9 = makePersonWithSlashDates "Smith" "Steve" "Male" "3/3/1985" "Red"

ps = [p2,p1,p3,p4,p6,p5,p9,p7,p8]
