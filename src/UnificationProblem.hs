{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module UnificationProblem where

import Data.List

import Term 
import NameGen
import Eval
import Constructor 
import TypeCheck
import Pretty


data Constraint
 = Failed Constraint -- goes first here so when you sort it comes first
 | T :?= T
 deriving(Eq, Ord, Show)

instance Pretty Constraint where
  pretty (Failed a) = "Failed(" ++ pretty a ++ ")"
  pretty (a :?= b) = pretty a ++ " ?= " ++ pretty b

instance HasTerms Constraint where
  modifyTerms f c = go c where
    go (Failed c) = Failed (modifyTerms f c)
    go (a :?= b)  = f a :?= f b
  collectTerms c = go c where
    go (Failed c) = collectTerms c
    go (a :?= b)  = collectTerms a ++ collectTerms b

data UnificationProblem = UnificationProblem
 { mainTerm :: T
 , metas :: [C B]
 , constraints :: [C Constraint]
 , entropy :: Int -- times 100 
 , parent :: Maybe UnificationProblem
 , redundancy :: Int
 } deriving(Eq, Show)

instance Ord UnificationProblem where
  compare up1 up2 = compare (heuristic up1) (heuristic up2)

heuristic :: UnificationProblem -> Int
heuristic up = size (mainTerm up) * (1 + length (history up)) * entropy up

totalSize :: UnificationProblem -> Int
totalSize up = size (mainTerm up) + length (metas up)

instance Pretty UnificationProblem where
  pretty up@(UnificationProblem {mainTerm, metas, constraints, entropy, redundancy}) = 
    "Entropy: "         ++ show entropy               ++ "\n" ++ 
    "Redundancy: "      ++ show redundancy            ++ "\n" ++
    "Age: "             ++ show (length (history up)) ++ "\n\n" ++ 
    "Main Term: "       ++ pretty mainTerm            ++ "\n\n" ++ 
    "Meta Vars:\n\n"    ++ pretty metas               ++ "\n\n" ++
    "Constraints: \n\n" ++ pretty constraints 

instance HasTerms UnificationProblem where
  modifyTerms f (UnificationProblem a b c d e g)
    = UnificationProblem (modifyTerms f a) (modifyTerms f b) (modifyTerms f c) d e g
  collectTerms (UnificationProblem a b c d e f) 
    = concat [collectTerms a, collectTerms b, collectTerms c]


initUnificationProblem :: T -> UnificationProblem
initUnificationProblem t = UnificationProblem 
  { mainTerm = M "X" [] [] False
  , metas = [[] :- "X" :. t]
  , constraints = []
  , entropy = 0
  , parent = Nothing
  , redundancy = 0 
  }

history :: UnificationProblem -> [UnificationProblem]
history up = case parent up of 
  Just up' -> up : history up' 
  Nothing -> [] 

isFinished :: UnificationProblem -> Bool
isFinished up = null (metas up) && null (constraints up)

isFailed :: UnificationProblem -> Bool
isFailed up = 
  case sort $ map (\(_ :- c) -> c) (constraints up) of 
    Failed _ : _ -> True
    _ -> False
