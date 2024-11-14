{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Pretty where

import Data.List

import Term

class Pretty a where
  pretty :: a -> String

instance Pretty B where
  pretty (x :. y) = "(" ++ x ++ ":"  ++ pretty y ++ ")"
  pretty (x := y) = "{" ++ x ++ "="  ++ pretty y ++ "}"

instance Pretty T where
  pretty e = go e where
    go (a :@@@ bs) = go a ++ " " ++ intercalate " " (map addParens bs)
    go (a :<<< bs) = addParens a ++ intercalate "" (map f bs) where 
      f (x :. y) = "(" ++ x ++ ":"  ++ go y ++ ")"
    go (a :\\\ bs) = addParens a ++ intercalate "" (map f bs) where 
      f (x :. y) = "[" ++ x ++ ":"  ++ go y ++ "]"
    go (K k) = take k (repeat '#')
    go (C n) = "'" ++ n
    go (V n) = n 
    go (M n frees subs _) = "?" ++ n ++ prettySubs where 
      prettySubs = concat $ map pretty subs 
    go e = show e 

addParens :: T -> String
addParens e 
  | size e > 1 = inParens (pretty e)
  | otherwise = pretty e

inParens, inBrackets, inBraces :: String -> String
inParens s   = "(" ++ s ++ ")"
inBrackets s = "[" ++ s ++ "]"
inBraces s   = "{" ++ s ++ "}"

instance Pretty a => Pretty (C a) where
  pretty (c :- a) = pretty a ++ "\n" ++ indentLines (pretty c)

indentLines :: String -> String
indentLines = unlines . map ("  " ++) . lines 

instance Pretty Int where
  pretty = show

instance {-# OVERLAPPING #-} Pretty String where
  pretty s = s

instance Pretty a => Pretty [a] where
  pretty a = indentLines $ unlines $ map pretty a

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = indentLines $ unlines $ [pretty a, pretty b]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (a, b, c) = indentLines $ unlines $ [pretty a, pretty b, pretty c]

instance Pretty a => Pretty (Maybe a) where
  pretty (Just a) = pretty a
  pretty Nothing = "<Nothing>"

