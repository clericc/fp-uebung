module TrieZipper where

import Data.AssocList
import Data.Char

-- ------------------------------------------------------
-- syntactic domains

data Trie i a = Trie (Maybe a) [(i,Trie i a)]

data TrieCtx i a = Top
                 | Sub (Maybe a) [(i,Trie i a)] (TrieCtx i a)

type TrieLoc  i a = (Trie i a, TrieCtx i a)
type StringTrie a = TrieCtx Char a
type StringLoc  a = TrieLoc Char a

-- ------------------------------------------------------
-- helper instances

instance (Show i, Show a) => Show (Trie i a) where
  show = show' ""
    where
      show' indent (Trie  Nothing []) = " => "
      show' indent (Trie (Just v) []) = " => " ++ show v
      show' indent (Trie  Nothing xs)
        = xs >>= \ (k,v) ->
            "\n" ++ indent ++ "-- " ++ (show k) ++ "" ++ show' (indent ++ "  ") v

      show' indent (Trie (Just v) xs)
        = indent ++ "=> " ++ show v ++ show' indent (Trie Nothing xs)


-- ------------------------------------------------------
-- functions on tries

emptyTrie :: Eq k => Trie k a
emptyTrie  = Trie Nothing []

insertTrie :: Eq i => [i] -> a -> Trie i a -> Trie i a
insertTrie     [] e (Trie value children)
  = Trie (Just e) children
insertTrie (k:ks) e (Trie value children)
  = case lookup k children of
      Nothing      ->
        Trie value (addEntry k (insertTrie ks e emptyTrie) children)
      Just subtree ->
        Trie value (addEntry k (insertTrie ks e subtree) children)

find :: Eq i => [i] -> Trie i a -> Maybe a
find [] (Trie value _)
  = value
find (k:ks) (Trie _ children)
  = case lookup k children of
      Nothing -> Nothing
      Just subtree -> find ks subtree

-- ------------------------------------------------------
-- zipper functions on tries

top :: Trie k v -> TrieLoc k v
top trie = (trie, Top)

down :: k -> TrieLoc k v -> TrieLoc k v
down k



trie1 = foldr (\ (k,v) -> (insertTrie k v .) ) id

  [("", "emptystring")
  ,("a",   "a")
  ,("b",   "b")
  ,("aa", "aa")
  ,("ab", "ab")
  ,("bb", "bb")
  ,("eins", "eins")
  ]

  $ emptyTrie

