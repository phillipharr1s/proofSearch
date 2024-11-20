{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Term where

import Data.List

type N = String

data B
 = N :. T -- x : X
 | N := T -- x = X 
 deriving (Eq, Ord, Show)

getName :: B -> N 
getName (n :. _) = n
getName (n := _) = n 

getTerm :: B -> T
getTerm (_ :. t) = t
getTerm (_ := t) = t 

infixl 6 :.

data T
 = T :< B -- pi
 | T :\ B -- lambda
 | T :@ T -- application
 | V N -- variable
 | C N -- constructor
 | K Int -- kind
 | M N [N] [B] Bool -- metavariable (name) (free variables) (substitutions) (redexFlag)
 deriving(Eq, Ord, Show)

infixl 4 :@
infixl 5 :<
infixl 5 :\ 

data C a = [B] :- a deriving(Eq, Ord, Show)

dropContext :: C a -> a
dropContext (_ :- a) = a

infixl 4 :-

getBinder (a :< n :. b) = Just ((:<), a, n, b)
getBinder (a :\ n :. b) = Just ((:\), a, n, b)
getBinder _ = Nothing

pattern Binder binder a n b <- (getBinder -> Just (binder,a,n,b)) where
  Binder binder a n b = a `binder` (n :. b) 

getApps e = go e [] where
  go (a :@ b) xs = go a (b : xs)
  go e xs = (e, xs)

putApps a as = foldl (:@) a as

infixl 4 :@@
pattern a :@@ as <- (getApps -> (a, as)) where
  a :@@ as = putApps a as

infixl 4 :@@@
pattern a :@@@ as <- a :@@ as@(_:_)

getPis e = go e [] where
  go (a :< b) xs = go a (b : xs)
  go e xs = (e, xs)

putPis a as = foldl (:<) a as

infixl 4 :<<
pattern a :<< as <- (getPis -> (a, as)) where
  a :<< as = putPis a as

infixl 4 :<<<
pattern a :<<< as <- a :<< as@(_:_)

getLams e = go e [] where
  go (a :\ b) xs = go a (b : xs)
  go e xs = (e, xs)

putLams a as = foldl (:\) a as

infixl 4 :\\
pattern a :\\ as <- (getLams -> (a, as)) where
  a :\\ as = putLams a as

infixl 4 :\\\
pattern a :\\\ as <- a :\\ as@(_:_)

collectMetaVars :: T -> [T]
collectMetaVars e = go e where
  go (a :@ b) = go a ++ go b
  go (a :< _ :. b) = go a ++ go b
  go (a :\ _ :. b) = go a ++ go b
  go meta@(M _ _ subs _) = meta : concatMap (collectMetaVars.getTerm) subs 
  go _ = []

metaVarNames = map (\(M n _ _ _) -> n) . collectMetaVars

isGroundTerm :: T -> Bool
isGroundTerm e = null (collectMetaVars e)

collectFreeVars :: T -> [N]
collectFreeVars e = sort $ nub $ go e where
  go (a :@ b) = go a ++ go b
  go (a :< n :. b) = filter (/= n) (go a) ++ go b
  go (a :\ n :. b) = filter (/= n) (go a) ++ go b
  go (V n) = [n]
  go _ = []

isClosedTerm :: T -> Bool 
isClosedTerm e = null (collectFreeVars e)

size :: T -> Int
size (e1 :@ e2) = size e1 + size e2
size (e1 :< _ :. e2) = size e1 + size e2
size (e1 :\ _ :. e2) = size e1 + size e2
size (M _ _ subs _) = 1 + sum (map size $ collectTerms subs)
size (K (-1)) = 0 
size _ = 1


class HasTerms a where
  modifyTerms  :: (T -> T) -> (a -> a)
  collectTerms :: a -> [T] 

instance HasTerms T where
  modifyTerms f t = f t
  collectTerms t = [t] 

instance HasTerms B where
  modifyTerms f (a :. t) = a :. f t
  modifyTerms f (a := t) = a := f t

  collectTerms (a :. t) = [t] 
  collectTerms (a := t) = [t] 

instance HasTerms a => HasTerms [a] where
  modifyTerms f as = map (modifyTerms f) as 
  collectTerms as = concatMap collectTerms as

instance HasTerms a => HasTerms (C a) where 
  modifyTerms f (c :- a) = modifyTerms f c :- modifyTerms f a
  collectTerms (c :- a) = collectTerms a ++ collectTerms c

instance (HasTerms a, HasTerms b) => HasTerms (a,b) where
  modifyTerms f (a, b) = (modifyTerms f a, modifyTerms f b) 
  collectTerms (a, b) = collectTerms a ++ collectTerms b

instance (HasTerms a, HasTerms b, HasTerms c) => HasTerms (a,b,c) where
  modifyTerms f (a, b, c) = (modifyTerms f a, modifyTerms f b, modifyTerms f c) 
  collectTerms (a, b, c) = collectTerms a ++ collectTerms b ++ collectTerms c
