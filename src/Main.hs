module Main where

import Data.List
import qualified Data.PQueue.Min as Queue

import Control.Monad

import System.IO

import Term 
import NameGen
import Eval
import Constructor 
import TypeCheck

import Parse
import Pretty

import UnificationProblem
import Grafting
import Search

main :: IO ()
main = showSearch additionCommutes

p :: Pretty a => a -> IO ()
p = putStrLn . pretty 


eqCongruence = q " ('eq X (f x) (f y))(e : 'eq X x y)(f:X(_:X))(y:X)(x:X)(X:#) "
additionLeftIdentity = q " ('eq 'N ('+ 'Z y) y)(y:'N) " 
additionCommutes = q " ('eq 'N ('+ x y) ('+ y x))(y:'N)(x:'N)" 
additionCancels = q "('eq 'N a b)(eq: 'eq 'N ('+ a x) ('+ b x))(x:'N)(b:'N)(a:'N)"

showSearch :: T -> IO ()
showSearch term = go 0 (initSearch $ initUnificationProblem term) where
  go i (finished : _ , unfinished) = do 
    putStrLn "Solution Found: " 
    putStrLn $ pretty (mainTerm finished)
  go i searchState@([], unfinished) = do
    hSetBuffering stdout (BlockBuffering Nothing)
    putStrLn "\n\n\n\n\n\n\n\n\n"
    putStrLn $ "Iteration " ++ show i ++ "\nUnfinished terms: \n"
    forM_ (Queue.take 10 unfinished) $ \up -> do
      putStrLn $ pretty (mainTerm up) ++ "\n"
    when (i `mod` 100 == 0)
      $ putStrLn $ pretty (Queue.getMin unfinished)
    putStrLn $ "Plus " ++ show (max 0 (Queue.size unfinished - 10)) ++ " more..."
    hFlush stdout
    go (i+1) (searchStep searchState)




