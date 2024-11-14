{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Search where

import Data.List
import qualified Data.PQueue.Min as Queue
import Data.PQueue.Min (MinQueue)

import Term 
import NameGen
import Eval
import Constructor 
import TypeCheck

import UnificationProblem
import Grafting



type SearchState = ([UnificationProblem], (MinQueue UnificationProblem))

initSearch up = ([], Queue.singleton up)

searchStep ss@(finished, unfinished) = case Queue.minView unfinished of 
  Just (up, unfinished') -> show newUnfinished `seq`
    (finished ++ newFinished, foldr Queue.insert unfinished' (addRedundancy newUnfinished)) where
      (newFinished, newUnfinished) = 
        partition isFinished (getBranches up)
      addRedundancy ups = map (\up -> up { redundancy = r }) ups where 
        up = head ups -- the value will be basically the same for all up in a branch 
        r = sum [roughSimilarity (mainTerm up) (mainTerm up') | up' <- sample] `div` (1+length sample)
      sample = [ (Queue.!!) unfinished i | i <- sampleIndices ]
      sampleIndices = takeWhile (< Queue.size unfinished) [3^k | k <- [0..] ] 
  Nothing -> ss
 

getBranches :: UnificationProblem -> [UnificationProblem]
getBranches up = concatMap doObviousGraftings $ takeSmallestBranch up

takeSmallestBranch :: UnificationProblem -> [UnificationProblem]
takeSmallestBranch up = head' $ sortOn length $ applyAllElementaryGraftings up where
  head' (a : as) = a
  head' [] = []

doObviousGraftings :: UnificationProblem -> [UnificationProblem]
doObviousGraftings up = case obviousGraftings up of 
  g:_ -> concatMap doObviousGraftings $     
    filter (not.isFailed) $
    map simplifyConstraints $ 
    [applyElementaryGrafting up g]
  [] -> [up] 

roughSimilarity :: T -> T -> Int
roughSimilarity a b = go a b where
  go (a :@ b) (a' :@ b') = go a a' + go b b' 
  go (a :< _ :. b) (a' :< _ :. b') = go a a' + go b b' 
  go (a :\ _ :. b) (a' :\ _ :. b') = go a a' + go b b'
  go (C a) (C b) | a == b = 1 
  go (V a) (V b) | a == b = 1
  go (K a) (K b) | a == b = 1
  go (M a _ _ _) (M b _ _ _) | a == b = 1 
  go _ _ = 0




applyAllElementaryGraftings :: UnificationProblem -> [[UnificationProblem]]
applyAllElementaryGraftings up = res where
  res = [ go graftingSet | graftingSet <- allElementaryGraftings up]
  go graftingSet = 
    changeMetadata $ 
    filter (not.isFailed) $
    map simplifyConstraints $ 
    [ applyElementaryGrafting up g | g <- graftingSet]
  changeMetadata set = 
    map (\up' -> up' {entropy = entropy up + log2 (length set), parent = Just up}) set 
  log2 x = round (100.0 * log (fromIntegral x) / log 2)

