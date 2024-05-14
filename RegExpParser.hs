module RegExpParser (RegExp (..), parseRegExp) where

import Text.Parsec
import Text.Parsec.String

data RegExp
  = -- | A character that is not in "()*|."
    Normal Char
  | -- | Any character
    Any
  | -- | Zero or more occurances of the same regexp
    ZeroOrMore RegExp
  | -- | A choice between 2 regexps
    Or RegExp RegExp
  | -- | A sequence of regexps.
    Str [RegExp]
  deriving (Show, Eq)

regExp :: Parser RegExp
regExp = do
  x <- seqs
  my <- optionMaybe (char '|' *> seqs)
  return $ case my of
    Just y -> Or x y
    Nothing -> x

seqs :: Parser RegExp
seqs = do
  xs <- many1 term
  return $ case xs of
    [x] -> x
    _ -> Str xs

term :: Parser RegExp
term = do
  x <- factor
  my <- optionMaybe (char '*')
  return $ case my of
    Just _ -> ZeroOrMore x
    Nothing -> x

factor :: Parser RegExp
factor =
  choice
    [ Any <$ char '.'
    , Normal <$> noneOf "()*|."
    , between (char '(') (char ')') regExp
    ]

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

parseRegExp :: String -> Maybe RegExp
parseRegExp = eitherToMaybe . parse (regExp <* eof) ""