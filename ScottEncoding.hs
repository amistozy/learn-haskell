{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScottEncoding where

import Prelude hiding (concat, curry, foldl, foldr, fst, length, map, null, snd, take, uncurry, zip, (++))

newtype SMaybe a = SMaybe {runMaybe :: forall b. b -> (a -> b) -> b}
newtype SList a = SList {runList :: forall b. b -> (a -> SList a -> b) -> b}
newtype SEither a b = SEither {runEither :: forall c. (a -> c) -> (b -> c) -> c}
newtype SPair a b = SPair {runPair :: forall c. (a -> b -> c) -> c}

toPair :: SPair a b -> (a, b)
toPair x = runPair x (,)
fromPair :: (a, b) -> SPair a b
fromPair (x, y) = SPair $ \f -> f x y
fst :: SPair a b -> a
fst x = runPair x const
snd :: SPair a b -> b
snd x = runPair x (const id)
swap :: SPair a b -> SPair b a
swap x = runPair x $ \a b -> SPair $ \f -> f b a
curry :: (SPair a b -> c) -> (a -> b -> c)
curry f x y = f $ SPair $ \f -> f x y
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f x = runPair x f

toMaybe :: SMaybe a -> Maybe a
toMaybe x = runMaybe x Nothing Just
fromMaybe :: Maybe a -> SMaybe a
fromMaybe x = SMaybe $ \b f -> maybe b f x
isJust :: SMaybe a -> Bool
isJust x = runMaybe x False (const True)
isNothing :: SMaybe a -> Bool
isNothing x = runMaybe x True (const False)
catMaybes :: SList (SMaybe a) -> SList a
catMaybes xs =
  runList xs (SList const) $ \x xs' ->
    runMaybe x id cons (catMaybes xs')

toEither :: SEither a b -> Either a b
toEither x = runEither x Left Right
fromEither :: Either a b -> SEither a b
fromEither x = SEither $ \l r -> either l r x
isLeft :: SEither a b -> Bool
isLeft x = runEither x (const True) (const False)
isRight :: SEither a b -> Bool
isRight x = runEither x (const False) (const True)
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition xs =
  runList xs (SPair $ \f -> f (SList const) (SList const)) $ \x xs' ->
    runEither
      x
      (\a p -> runPair p $ \as bs -> SPair $ \f -> f (cons a as) bs)
      (\b p -> runPair p $ \as bs -> SPair $ \f -> f as (cons b bs))
      (partition xs')

toList :: SList a -> [a]
toList xs = runList xs [] (\a xs' -> a : toList xs')
fromList :: [a] -> SList a
fromList [] = SList const
fromList (x : xs) = SList $ \_ f -> f x (fromList xs)
cons :: a -> SList a -> SList a
cons x xs = SList $ \_ f -> f x xs
concat :: SList a -> SList a -> SList a
concat xs ys = runList xs ys (\x xs' -> cons x (concat xs' ys))
null :: SList a -> Bool
null xs = runList xs True (const $ const False)
length :: SList a -> Int
length xs = runList xs 0 (\_ xs' -> 1 + length xs')
map :: (a -> b) -> SList a -> SList b
map f xs = runList xs (SList const) (\x xs' -> cons (f x) (map f xs'))
zip :: SList a -> SList b -> SList (SPair a b)
zip xs ys =
  runList xs (SList const) $ \x xs' ->
    runList ys (SList const) $ \y ys' ->
      cons (SPair $ \f -> f x y) (zip xs' ys')
foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f b as = runList as b (foldl f . f b)
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f b as = runList as b (\a as' -> f a (foldr f b as'))
take :: Int -> SList a -> SList a
take 0 xs = SList const
take n xs = runList xs (SList const) (\x xs' -> cons x (take (n - 1) xs'))
