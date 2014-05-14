module ParsecParser where

import Text.ParserCombinators.Parsec
import Person

commaLine = "Abercrombie, Neil, Male, Tan, 2/13/1943\nBishop, Timothy, Male, Yellow, 4/23/1967\nKelly, Sue, Female, Pink, 7/12/1959"

csvFile = sepEndBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

parseCSV :: String -> Either ParseError [Person]
parseCSV = parse csvFile "(unknown)"
