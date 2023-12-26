module JsonParser where

import Control.Applicative
import Data.Char
import Data.Functor

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }
  deriving (Functor)

-- instance Functor Parser where
--   fmap f (Parser p) =
--     Parser $ \input -> do
--       (input', x) <- p input
--       return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input ->
      p1 input <|> p2 input

jsonNull :: Parser JsonValue
jsonNull = stringP "null" $> JsonNull

charP :: Char -> Parser Char
charP x = Parser f
 where
  f [] = Nothing
  f (y : ys)
    | y == x = Just (ys, x)
    | otherwise = Nothing

stringP :: String -> Parser String
stringP = traverse charP

jsonBool :: Parser JsonValue
jsonBool =
  (stringP "true" $> JsonBool True)
    <|> (stringP "false" $> JsonBool False)

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
   in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> notNull (spanP isDigit)

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
 where
  elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> ws *> pairs <* ws <* charP '}')
 where
  pairs = sepBy (ws *> charP ',' <* ws) pair
  pair = (,) <$> stringLiteral <* (ws *> charP ':' <* ws) <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject
