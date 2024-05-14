{-# LANGUAGE LambdaCase #-}

module TinyThreePassCompiler where

import Data.List (elemIndex)
import Text.Parsec hiding (char, digit)

type Parser = Parsec [Token] ()

myToken :: (Token -> Maybe a) -> Parser a
myToken = tokenPrim show (const . const)

data AST
  = Imm Int
  | Arg Int
  | Add AST AST
  | Sub AST AST
  | Mul AST AST
  | Div AST AST
  deriving (Eq, Show)

data Token
  = TChar Char
  | TInt Int
  | TStr String
  deriving (Eq, Show)

alpha, digit :: String
alpha = ['a' .. 'z'] ++ ['A' .. 'Z']
digit = ['0' .. '9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c : cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
 where
  (i, is) = span (`elem` digit) xxs
  (s, ss) = span (`elem` alpha) xxs

parseFunc :: [Token] -> AST
parseFunc (TChar '[' : ts) = ast
 where
  (args, _ : ts') = span (/= TChar ']') ts
  argList = map (\(TStr x) -> x) args
  Right ast = parse exp "" ts'
  exp = term `chainl1` addop
  term = factor `chainl1` mulop
  addop = (Add <$ char '+') <|> (Sub <$ char '-')
  factor = number <|> var <|> between (char '(') (char ')') exp
  mulop = (Mul <$ char '*') <|> (Div <$ char '/')
  number = myToken $ \case
    TInt x -> Just $ Imm x
    _ -> Nothing
  var = myToken $ \case
    TStr x -> Arg <$> elemIndex x argList
    _ -> Nothing
  char c = myToken $ \x -> if x == TChar c then Just c else Nothing

eval :: (AST -> AST -> AST) -> (Int -> Int -> Int) -> AST -> AST -> AST
eval op f x y =
  case (pass2 x, pass2 y) of
    (Imm a, Imm b) -> Imm (f a b)
    (c, d) -> op c d

comp :: String -> AST -> AST -> [String]
comp op x y = pass3 x ++ ["PU"] ++ pass3 y ++ ["SW", "PO", op]

compile :: String -> [String]
compile = pass3 . pass2 . pass1

pass1 :: String -> AST
pass1 = parseFunc . tokenize

pass2 :: AST -> AST
pass2 (Add x y) = eval Add (+) x y
pass2 (Sub x y) = eval Sub (-) x y
pass2 (Mul x y) = eval Mul (*) x y
pass2 (Div x y) = eval Div div x y
pass2 x = x

pass3 :: AST -> [String]
pass3 (Imm x) = ["IM " ++ show x]
pass3 (Arg x) = ["AR " ++ show x]
pass3 (Add x y) = comp "AD" x y
pass3 (Sub x y) = comp "SU" x y
pass3 (Mul x y) = comp "MU" x y
pass3 (Div x y) = comp "DI" x y