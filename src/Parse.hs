module Parse where

import Text.Parsec
import Text.Parsec.Char

import Term

type Parse a = Parsec String () a

parseName = many1 (oneOf ".<>!@$%^&*-_+/" <|> letter <|> digit)

parseK = K . length <$> many1 (char '#')
parseV = V <$> parseName
parseC = C              <$> (char '\'' *> parseName)
parseM = (\n -> M n [] [] True) <$> (char '?'  *> parseName)

parseEq = do 
  V a <- parseT <* spaces
  char '=' <* spaces
  b <- parseT
  pure $ a := b

parseAtom = (parseK <|> parseV <|> parseC <|> parseM) <* spaces

inside :: (Parse a, Parse b) -> Parse c -> Parse c
inside (start, end) p = between (start <* spaces) (end <* spaces) p

parens = (char '(', char ')')
brackets = (char '[', char ']')
braces = (char '{', char '}')

parseBind :: Parse B
parseBind = do
  n <- parseName <* spaces
  char ':' <* spaces
  e <- parseT
  pure $ n :. e

data Block = PiBlock B | LambdaBlock B | SubBlock B | TermBlock T

parseBlock = 
      try (PiBlock      <$> inside parens   parseBind) 
  <|> try (LambdaBlock  <$> inside brackets parseBind) 
  <|> try (SubBlock     <$> inside braces parseEq) 
  <|> try (TermBlock    <$> inside parens parseT)
  <|>     (TermBlock    <$> parseAtom)

processBlocks (TermBlock b : bs) = go b bs where
  go e (TermBlock b   : bs)  = go (e :@ b) bs
  go e (PiBlock b     : bs)  = go (e :< b) bs
  go e (SubBlock b    : bs)  = go (addSub e b) bs
  go e (LambdaBlock b : bs)  = go (e :\ b) bs
  go e [] = e

  addSub (M n frees ss redexFlag) s = M n frees (s:ss) redexFlag
  addSub e s = error $ "addSub" ++ show e 

parseT :: Parse T
parseT = do
  blocks <- many1 parseBlock <* spaces
  pure $ processBlocks blocks

quickParse :: String -> T
quickParse s = case parse (spaces *> parseT <* eof) "" s of 
  Right e -> e
  Left e -> error (show e) 

q :: String -> T
q = quickParse