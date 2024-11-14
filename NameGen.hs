{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module NameGen where

import Data.Char

getFreshName :: [String] -> [String] -> String
getFreshName avoidSet nameStream = head $ dropWhile (\n -> elem n avoidSet) nameStream 

addUnderscores :: String -> [String]
addUnderscores name = name : addUnderscores ("_" ++ name)

valueNames :: String -> [String]
valueNames name = (map.map) toLower (mutateNameStream name)

typeNames :: String -> [String]
typeNames name  = (map.map) toUpper (mutateNameStream name)

mutateNameStream :: String -> [String]
mutateNameStream name = go [name] where
  go names = names ++ go (concatMap mutateName names)

mutateName :: String -> [String]
mutateName name@(c : cs) = 
  [name]
  ++ [ c' : cs | c' <- mutateChar c] 
  ++ [ c: cs' | cs' <- mutateName cs]
  ++ [name ++ "1" | not $ isNumber $ last name ]
mutateName [] = [] 

mutateChar :: Char -> [Char]
mutateChar char = filter isAlphaNum $ map chr [n + 1, n - 1] where n = ord char 
