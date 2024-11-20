
module TypeCheck where

import Term 
import Eval 
import Constructor 

data TypeCheckError
  = NotKind T
  | NotPi T 
  deriving(Eq, Ord, Show)

-- This is a bit too strict:
-- It assumes a meta-variable can't be a kind or a Pi
typeCheckWithMetas :: [C B] -> [B] -> T -> Either TypeCheckError T 
typeCheckWithMetas metas c e = go c e where
  go c (a :@ b) = do
    ta <- go c a 
    tb <- go c b
    case ta of
      x :< n :. y -> Right $ nf $ ta :@ b
      _ -> Left $ NotPi ta
  go c (a :\ n :. b) = do
    ta <- go (n :. b : c) a
    tb <- go c b
    pure (ta :< n :. b)
  go c (a :< n :. b) = do 
    ta <- go (n :. b : c) a
    tb <- go c b
    case ta of
      K i -> pure $ K i
      _ -> Left (NotKind ta) 
  go c (M n _ subs _) = 
    pure $ applySubList (getTerm $ dropContext $ lookupMeta n metas) subs 
  go c atom = pure $ lookupAtomType c atom 

lookupMeta :: N -> [C B] -> C B
lookupMeta n = head . filter (\(_ :- n' :. _) -> n == n')

lookupVar :: N -> [B] -> T 
lookupVar n = getTerm . head . filter (\(n' :. _) -> n == n')

lookupAtomType :: [B] -> T -> T
lookupAtomType c e = case e of
  K i -> K (i+1) 
  V n -> lookupVar n c
  C a -> case typeConstructor a of 
    K (-1) -> lookupVar a allAliasesContext
    t -> t 
  e -> error $ show e 

allAliasesContext :: [B]
allAliasesContext = go [] allAliases where
  go c ((n,d):ds) = go (n :. t : c) ds where
    Right t = typeCheckWithMetas [] c d
  go c [] = c