{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Grafting where

import Term 
import NameGen
import Eval
import Constructor 
import TypeCheck
import Pretty
import UnificationProblem

type Grafting = (N, T)

graft :: N -> T -> T -> T
graft x y e = go e where
  go (a :@ b) = go a :@ go b
  go e@(Binder binder a n b) = Binder binder (go a) n (go b) 
  go (M n frees subs redexFlag) 
    | n == x = graft x y $ applySubList y subs
    | otherwise = M n frees (map (modifyTerms (graft x y)) subs) redexFlag
  go e = e

applyGrafting :: Grafting -> UnificationProblem -> UnificationProblem
applyGrafting (name, body) problem = 
  deleteMeta name $ modifyTerms (graft name body) problem 

type ElementaryGrafting = ([C B], [C Constraint], Grafting)

applyElementaryGrafting :: UnificationProblem -> ElementaryGrafting -> UnificationProblem
applyElementaryGrafting up (newMetas, newConstraints, grafting) = 
  modifyTerms nf $ 
  applyGrafting grafting $ up 
    { metas = newMetas ++ metas up
    , constraints = newConstraints ++ constraints up
    }

obviousGraftings :: UnificationProblem -> [ElementaryGrafting]
obviousGraftings up = concatMap go (cons ++ map flipConstraint cons) where 
  cons = constraints up 
  go (_ :- a :?= b) = case (a, b) of 
    (M n _ [] _, b ) | not (elem n $ metaVarNames b) && isGroundTerm b -> [([],[],(n, b))]
    _ -> []

allElementaryGraftings :: UnificationProblem -> [[ElementaryGrafting]]
allElementaryGraftings up = 
  [ elementaryGraftingsForMeta meta up | meta <- metas up ]

elementaryGraftingsByName :: N -> UnificationProblem -> [ElementaryGrafting]
elementaryGraftingsByName name up = 
  elementaryGraftingsForMeta (lookupMeta name (metas up)) up 

elementaryGraftingsForMeta :: C B -> UnificationProblem -> [ElementaryGrafting]
elementaryGraftingsForMeta meta@(c :- n :. t) (UnificationProblem {mainTerm, metas, constraints}) = 
  onlySplitLamIfPossible
    where

      onlySplitLamIfPossible = case splitLam of 
        [] -> splitPi ++ splitK1 ++ allHeadSplits
        a -> a

      allNames :: [N]
      allNames = map (getName.dropContext) metas ++ map getName c

      splitPi :: [ElementaryGrafting]
      splitPi = case t of 
        K _ -> [(newMetas, [], (n, body :< argName :. argType))]  where
          bodyName = getFreshName allNames (typeNames n)
          argTypeName = getFreshName (bodyName : allNames) (typeNames n)
          argName = getFreshName 
            ([bodyName , argTypeName] ++ allNames) 
            ((if t == K 1 then valueNames else typeNames) argTypeName)
          (argTypeDec, argType) = initMeta c argTypeName t
          (bodyDec, body) = initMeta (argName :. argType : c) bodyName t
          newMetas = [argTypeDec, bodyDec]
        _ -> [] 

      splitLam :: [ElementaryGrafting]
      splitLam = case t of 
        bodyType :< argName :. argType -> 
          [([bodyDec],[],(n, body :\ argName :. argType))] where
            bodyName = getFreshName (argName : allNames) (valueNames n)
            (bodyDec, body) = initMeta (argName :. argType : c) bodyName bodyType
        _ -> []

      splitK1 :: [ElementaryGrafting]
      splitK1 = case t of 
        K 2 -> [([], [], (n, K 1))]
        _ -> []

      allHeadSplits :: [ElementaryGrafting]
      allHeadSplits = do 
        (head,headType) <- boundVars ++ if avoidConstructorHead then [] else constructors
        i <- argNums headType
        splitHead i c head headType
          where
            boundVars = [ (V v, t) | v :. t <- c ]
            constructors = [ (C head, typeConstructor head) | head <- allConstructors]
            argNums (_ :<<< args) = map (+ length args) [-2, -1, 0, 1, 2]
            argNums _ = [0,1]

      splitHead :: Int -> [B] -> T -> T -> [ElementaryGrafting]
      splitHead i c head headType = do
        (newMetas, (m, tm)) <- imitations i c head headType
        pure (newMetas,[c :- t :?= tm], (n, setRedexFlag m))

      imitations i _ _ _ | i < 0 = [] 
      imitations 0 c head headType = [([], (head, headType))] 
      imitations i c head headType = do
        (newMetas, nb) <- imitations (i-1) c head headType
        go newMetas nb
          where
           go newMetas (n,b@(a2 :< a1Name :. a1)) = [([xDec] ++ newMetas, (n',b'))] where
            xName = getFreshName 
              (map (getName.dropContext) newMetas ++ allNames) 
              ((if a1 == K 1 then typeNames else valueNames) a1Name)
            (xDec, x) = initMeta c xName a1 
            n' = nf (n :@ x)
            b' = nf (b :@ x) 
           go newMetas (n, b) = []
      
      avoidConstructorHead :: Bool 
      avoidConstructorHead = case filter (\(M n' _ _ _) -> n == n') (collectMetaVars mainTerm) of
        M _ _ _ redexFlag : _ -> redexFlag
        _ -> False

      setRedexFlag :: T -> T 
      setRedexFlag t = case getEliminatorTarget t of
        Nothing -> t 
        Just target@(M targetName _ _ _) -> goSetRedexFlag targetName t 

      goSetRedexFlag :: N -> T -> T
      goSetRedexFlag targetName (a :@@ bs) = a :@@ map go bs where
        go (M n frees subs _) | n == targetName = M n frees subs True 
        go e = e 

                   
initMeta :: [B] -> N -> T -> (C B, T)
initMeta c name t = (c :- name :. t, M name (map (\(n :. _) -> n) c) [] False)

deleteMeta :: N -> UnificationProblem -> UnificationProblem
deleteMeta name up = up { metas = filter f (metas up) } where
  f (_ :- n :. _) = (n /= name)



simplifyConstraints :: UnificationProblem -> UnificationProblem
simplifyConstraints up = up { constraints = concatMap process (constraints up) } where
  process = go False . preProcess
  preProcess (c :- (a :?= b)) = c :- (nf a :?= nf b)
  go _ (c :- cons@((a :@@@ as) :?= (_ :\ _))) | rigidExpr (a :@@ as) = [c :- Failed cons]
  go _ (c :- cons@((a :@@@ as) :?= (_ :< _))) | rigidExpr (a :@@ as) = [c :- Failed cons]
  go _ (c :- cons@((_ :< _) :?= (_ :\ _))) = [c :- Failed cons]
  go _ (c :- cons@((a :< n :. b) :?= (a' :< n' :. b'))) = 
    process =<< [n :. b : c :- (a :?= sub n' (V n) a'), c :- (b :?= b')]
  go _ (c :- cons@((a :\ n :. b) :?= (a' :\ n' :. b'))) = 
    process =<< [n :. b : c :- (a :?= sub n' (V n) a'), c :- (b :?= b')]
  go _ (c :- cons@((a :@@ as) :?= (b :@@ bs))) | rigidExpr (a :@@ as) && rigidExpr (b :@@ bs) = 
    if length as /= length bs || a /= b
      then [c :- Failed cons]
      else process =<< map (c:-) (zipWith (:?=) as bs)
  go False (c :- (a :?= b)) = map flipConstraint $ go True (c :- (b :?= a))
  go True  (c :- (a :?= b)) = [c :- (a :?= b)]
  --TODO: eta conversion?

flipConstraint :: C Constraint -> C Constraint
flipConstraint (c :- a :?= b) = c :- b :?= a 
flipConstraint cons = cons 

rigidExpr :: T -> Bool
rigidExpr e = case e of
  M _ _ _ _ :@@ _ -> False 
  C _ :@@ _ -> case getEliminatorTarget e of 
    Just (M _ _ _ redexFlag) -> redexFlag
    _ -> True
  _ -> True 

