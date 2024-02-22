{-# LANGUAGE DeriveFunctor #-}

module OperatorParser (
  OpTree (..),
  Associativity (..),
  op,
  foldTree,
  parseOperators,
  module Text.ParserCombinators.ReadP,
)
where

import Control.Applicative hiding (many)
import Data.Char
import Data.Functor
import Text.ParserCombinators.ReadP

{- | Type for operator parse results. 'a' is the type of the operator, 'b'
| of the terms.
-}
data OpTree a b
  = Op (OpTree a b) a (OpTree a b)
  | Term b
  deriving (Show, Eq, Functor)

-- | Type for specifying the assocativity of operators: left, right, or none.
data Associativity a = L a | R a | NoAssociativity a
  deriving (Show, Eq, Functor)

-- | Transform an OpTree using the given function.
foldTree :: (a -> b -> b -> b) -> OpTree a b -> b
foldTree _ (Term x) = x
foldTree oxy (Op tree1 o tree2) = oxy o (foldTree oxy tree1) (foldTree oxy tree2)

{- | Return a parser such that: given 'op s a', if s matches, the parser
| returns a.
-}
op :: String -> a -> ReadP a
op s x = string s $> x

ws :: ReadP String
ws = many (satisfy isSpace)

layer :: Associativity [ReadP a] -> ReadP (OpTree a b) -> ReadP (OpTree a b)
layer (L ops) termP = chainl1 termP (flip Op <$> anyOp ops)
layer (R ops) termP = chainr1 termP (flip Op <$> anyOp ops)
layer (NoAssociativity ops) termP = (Op <$> termP <*> anyOp ops <*> termP) +++ termP

anyOp :: [ReadP a] -> ReadP a
anyOp ops = ws *> choice ops <* ws

{- | Accept two arguments:
| (1) A list of type [Associativity [ReadP a]], which contains parsers for
| operators (ReadP a). Each item of type Associativity [ReadP a] contains
| a group of operator parsers of the same precedence and associativity;
| these groups are listed in order of precedence (lowest to highest).
| (2) A parser for the terms.
| And return a parser for operator expressions that yields a parse tree.
-}
parseOperators :: [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
parseOperators assocs termP = loop
 where
  loop = foldr layer term assocs
  term = (Term <$> termP) +++ (char '(' *> ws *> loop <* ws <* char ')')