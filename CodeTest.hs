module CodeTest where

import Data.List (intercalate, sortBy)

data Gender = Male | Female deriving (Eq, Show)
type Name = String
type FirstName = Name
type LastName = Name
type Color = String
type DOB = String

data Person = Person FirstName LastName Gender Color DOB deriving (Eq)

instance Show Person where
  show (Person fn ln g c dob) = intercalate " " [ln, fn, show g, dob, c]

genderAndLastNameAscending :: Person -> Person -> Ordering
genderAndLastNameAscending (Person _ ln1 g1 _ _) (Person _ ln2 g2 _ _)
  | g1 == Female && g2 == Male = LT
  | g1 == Male && g2 == Female = GT
  | otherwise = compare ln1 ln2

lastNameDescending :: Person -> Person -> Ordering
lastNameDescending (Person _ ln1 _ _ _) (Person _ ln2 _ _ _) = compare ln2 ln1

sortByGenderAndLastNameAscending :: [Person] -> [Person]
sortByGenderAndLastNameAscending = sortBy genderAndLastNameAscending

sortByBirthDateAscending :: [Person] -> [Person]
sortByBirthDateAscending = undefined

sortByLastNameDescending :: [Person] -> [Person]
sortByLastNameDescending = sortBy lastNameDescending

-- UTILITY STUFF TO HELP WITH GHCI TESTING
makeGender :: String -> Gender
makeGender g
  | g == "Female" = Female
  | g == "Male" = Male

makePerson :: String -> String -> String -> String -> String -> Person
makePerson ln fn g dob c = Person fn ln (makeGender g) dob c

p1 = makePerson "Hingis" "Martina" "Female" "4/2/1979" "Green"
p2 = makePerson "Kelly" "Sue" "Female" "7/12/1959" "Pink"
p3 = makePerson "Kournikova" "Anna" "Female" "6/3/1975" "Red"
p4 = makePerson "Seles" "Monica" "Female" "12/2/1973" "Black"
p5 = makePerson "Abercrombie" "Neil" "Male" "2/13/1943" "Tan"
p6 = makePerson "Bishop" "Timothy" "Male" "4/23/1967" "Yellow"
p7 = makePerson "Bonk" "Radek" "Male" "6/3/1975" "Green"
p8 = makePerson "Bouillon" "Francis" "Male" "6/3/1975" "Blue"
p9 = makePerson "Smith" "Steve" "Male" "3/3/1985" "Red"

ps = [p2,p1,p3,p4,p6,p5,p9,p7,p8]
