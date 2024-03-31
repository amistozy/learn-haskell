module LispLovesMe where

import Data.Char (isDigit)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP

data AST
  = I32 Int
  | Sym String
  | Nul
  | Err
  | Lst [AST]
  | Boo Bool
  | Nod AST [AST]
  deriving (Eq, Show)

--

preludeFunctions :: [(String, [AST] -> AST)]
preludeFunctions =
  [ ("+", folder add)
  , ("*", folder mul)
  , ("-", folder sub)
  , ("/", folder div')
  , ("^", pow)
  , (">", gt)
  , ("<", lt)
  , ("!", not')
  , ("list", Lst)
  , ("size", size)
  , ("reverse", reverse')
  , ("..", range)
  , ("==", eq)
  , (">=", ge)
  , ("<=", le)
  , ("!=", notEq)
  , ("if", if')
  ]
 where
  folder f asts
    | length asts < 2 = Err
    | otherwise = foldl1 f asts

  add (I32 x) (I32 y) = I32 (x + y)
  add _ _ = Err

  mul (I32 x) (I32 y) = I32 (x * y)
  mul _ _ = Err

  sub (I32 x) (I32 y) = I32 (x - y)
  sub _ _ = Err

  div' (I32 x) (I32 y) = if y == 0 then Err else I32 (x `div` y)
  div' _ _ = Err

  pow [I32 x, I32 y] = I32 (x ^ y)
  pow _ = Err

  gt [I32 x, I32 y] = Boo (x > y)
  gt _ = Err

  lt [I32 x, I32 y] = Boo (x < y)
  lt _ = Err

  not' [Boo b] = Boo (not b)
  not' _ = Err

  size [Lst ls] = I32 (length ls)
  size _ = Err

  reverse' [Lst ls] = Lst (reverse ls)
  reverse' _ = Err

  range [I32 x, I32 y] = Lst $ map I32 [x .. y]
  range _ = Err

  eq [I32 x, I32 y] = Boo (x == y)
  eq _ = Err

  ge [I32 x, I32 y] = Boo (x >= y)
  ge _ = Err

  le [I32 x, I32 y] = Boo (x <= y)
  le _ = Err

  notEq [I32 x, I32 y] = Boo (x /= y)
  notEq _ = Err

  if' [Boo b, x] = if b then x else Nul
  if' [Boo b, x, y] = if b then x else y
  if' _ = Err

lispP :: ReadP AST
lispP = wsP *> choice [i32P, symP, nulP, nodP] <* wsP
 where
  i32P = I32 . read <$> munch1 isDigit
  symP = do
    s <-
      (:)
        <$> satisfy (`notElem` " ,\n\t\r()" ++ ['0' .. '9'])
        <*> munch (`notElem` " ,\n\t\r()")
    return $ case s of
      "null" -> Nul
      "true" -> Boo True
      "false" -> Boo False
      s -> Sym s
  nulP = Nul <$ string "()"
  nodP =
    Nod
      <$> (char '(' *> wsP *> lispP <* wsP)
      <*> (sepBy lispP wsP <* wsP <* char ')')
  wsP = munch (`elem` " ,\n\t\r")

--

parseLisp :: String -> Maybe AST
parseLisp s =
  case readP_to_S lispP s of
    [(ast, "")] -> Just ast
    _ -> Nothing

lispPretty :: String -> Maybe String
lispPretty = fmap pretty . parseLisp
 where
  pretty (I32 x) = show x
  pretty (Sym s) = s
  pretty Nul = "null"
  pretty (Boo b) = if b then "true" else "false"
  pretty (Nod a as) = "(" ++ unwords (map pretty (a : as)) ++ ")"

lispEval :: String -> Maybe AST
lispEval = fmap eval . parseLisp
 where
  eval (Nod (Sym s) asts) =
    case lookup s preludeFunctions of
      Nothing -> Err
      Just f -> f $ map eval asts
  eval ast = ast
