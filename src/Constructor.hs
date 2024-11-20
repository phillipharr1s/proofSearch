{-# LANGUAGE FlexibleInstances #-}

module Constructor where 

import Term
import Parse

reduceConstructor :: N -> [T] -> Maybe T
reduceConstructor head tail = go head tail where
  go "eqElim" (t : p : pf : a : b : (C "refl") : rest) = Just $ (pf :@ a) :@@ rest
  go "Nelim" (t : z : s : (C "S" :@ n) : rest) =
    Just $ s :@ n :@ (C "Nelim" :@ t :@ z :@ s :@ n) :@@ rest
  go "Nelim" (t : z : s : (C "Z") : rest) = Just $ z :@@ rest
  go head tail = (:@@ tail) <$> lookup head allAliases 

typeConstructor :: N -> T
typeConstructor head = case head of
  "eq" -> q "#(b:T)(a:T)(T:#)"
  "refl" -> q "('eq T a a)(a:T)(T:#)"
  "eqElim" -> q "(P a b eq)(eq: 'eq T a b)(b:T)(a:T)(pf:(P a a ('refl T a))(a:T))(P:#(e : 'eq T a b)(b:T)(a:T))(T:#)"
  "N" -> q "#"
  "Z" -> q "'N"
  "S" -> q "'N(_:'N)"
  "Nelim" -> q "(P n)(n:'N)(pfS:(P ('S n))(pn: P n)(n:'N))(pfZ : P 'Z)(P:#(n:'N))"
  other -> K (-1)

getEliminatorTarget :: T -> Maybe T
getEliminatorTarget (C a :@@ bs) = go a bs where
  go "Nelim" (t : z : s : targ : _) = Just targ
  go "eqElim" (t : p : pf : a : b : targ : _) = Just targ 
  go _ _ = Nothing
getEliminatorTarget _ = Nothing


allConstructors :: [N]
allConstructors = 
  [ "N" 
  , "S"
  , "Z"
  , "Nelim"
  ]
  ++ 
  [ "eq"
  , "refl" 
  , "eqElim"
  ]


allAliases :: [(N,T)]
allAliases = 
 [ ("+", q " 'Nelim ('N[_:'N]) m (('S rec)[rec : 'N][_ : 'N]) [m : 'N]")
 ]
