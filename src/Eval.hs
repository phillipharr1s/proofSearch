{-# LANGUAGE FlexibleInstances #-}

module Eval where

import Data.List

import Term
import NameGen
import Constructor

sub :: N -> T -> T -> T
sub x y e = go e where
  go (a :@ b) = go a :@ go b
  go e@(Binder binder a n b) 
   | n == x = Binder binder a n (go b)
   | elem n yFrees = go (alphaRename e yFrees)
   | otherwise = Binder binder (go a) n (go b) 
  go (V n) | n == x = y 
  go e@(M n mFrees subs redexFlag) 
   | null mFrees     = M n [] (x := y : subs) redexFlag
   | elem x mFrees = M n (nub $ yFrees ++ filter (/=x) mFrees) (x := y : subs) redexFlag
   | otherwise = e
  go e = e
  yFrees = collectFreeVars y 

applySubList :: T -> [B] -> T
applySubList y subs = foldr (\(x' := y') t -> sub x' y' t) y subs

alphaRename :: T -> [N] -> T
alphaRename (Binder binder a n b) avoidSet = 
  Binder binder (sub n (V n') a) n' b where
    n' = getFreshName avoidSet (addUnderscores n) 

whnf :: T -> T
whnf e = case reduce e of 
  Just e' -> whnf e'
  Nothing -> e 

reduce :: T -> Maybe T
reduce (Binder _ a n _ :@ arg) = Just (sub n arg a)
reduce (C a :@@ bs) = reduceConstructor a (map whnf bs)
reduce (a :@ b) = case reduce a of
  Just a' -> Just (a' :@ b)
  Nothing -> Nothing
reduce _ = Nothing

nf :: T -> T
nf e = case whnf e of
  a :< n :. b -> nf a :< n :. nf b
  a :\ n :. b -> nf a :\ n :. nf b
  a :@ b -> nf a :@ nf b
  e' -> e'

