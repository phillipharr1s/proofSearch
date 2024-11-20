module NameGen where

import Data.Char

import Text.Parsec
import Text.Parsec.Char

getFreshName :: [String] -> [String] -> String
getFreshName avoidSet nameStream = head $ dropWhile (`elem` avoidSet) nameStream 

addUnderscores :: String -> [String]
addUnderscores name = name : addUnderscores ("_" ++ name)

valueNames :: String -> [String]
valueNames name = (map.map) toLower (makeNameVariants name)

typeNames :: String -> [String]
typeNames name  = (map.map) toUpper (makeNameVariants name)

makeNameVariants :: String -> [String]
makeNameVariants s = case simpleName s of
  Right (c, i) -> [ [c'] | c' <- mutateChar c] ++ 
    concat [ [c' : show (i+j) | c' <- mutateChar c] | j <- [0..] ]
  Left _ -> addUnderscores s

mutateChar :: Char -> [Char]
mutateChar char = filter isAlpha [chr (n + i) | i <- [0..5]] where n = ord char 

simpleName :: String -> Either ParseError (Char, Int)
simpleName s = parse goParse "" s where 
  goParse = do
    c <- letter
    num <- (eof *> pure 0) <|> (read <$> many1 digit) 
    return (c, num)




