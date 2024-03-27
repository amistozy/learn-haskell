module Huffman (
  frequencies,
  encode,
  decode,
  Bit (..),
) where

import Data.Bifunctor (Bifunctor (first, second))
import Data.Function (on)
import Data.List

data Bit = Z | O deriving (Eq, Show)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

createHuffmanTree :: [(a, Int)] -> Tree a
createHuffmanTree freqs = go leafs
 where
  leafs = map (first Leaf) $ sortOn snd freqs
  go [(tree, _)] = tree
  go ((tree1, x) : (tree2, y) : nodes) = go $ insertBy (compare `on` snd) newNode nodes
   where
    newNode = (Node tree1 tree2, x + y)

getEncodeList :: Tree a -> [(a, [Bit])]
getEncodeList (Leaf x) = [(x, [])]
getEncodeList (Node ltree rtree) =
  map (second (Z :)) (getEncodeList ltree)
    ++ map (second (O :)) (getEncodeList rtree)

-- | Calculate symbol frequencies of a text.
frequencies :: (Ord a) => [a] -> [(a, Int)]
frequencies = map ((,) <$> head <*> length) . group . sort

-- | Encode a sequence using the given frequencies.
encode :: (Ord a) => [(a, Int)] -> [a] -> Maybe [Bit]
encode freqs xs
  | length freqs <= 1 = Nothing
  | otherwise = concat <$> traverse (`lookup` encodeList) xs
 where
  encodeList = getEncodeList $ createHuffmanTree freqs

-- | Decode a bit sequence using the given frequencies.
decode :: [(a, Int)] -> [Bit] -> Maybe [a]
decode freqs bits
  | length freqs <= 1 = Nothing
  | null bits = Just []
  | otherwise = go huffmanTree bits
 where
  huffmanTree = createHuffmanTree freqs
  go (Leaf x) [] = Just [x]
  go (Leaf x) bits = (x :) <$> go huffmanTree bits
  go (Node{}) [] = Nothing
  go (Node ltree _) (Z : bits) = go ltree bits
  go (Node _ rtree) (O : bits) = go rtree bits