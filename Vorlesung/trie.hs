module TrieZipper where

import Data.AssocList
import Data.Char

-- ------------------------------------------------------
-- syntactic domains

data Trie k v = Trie (Maybe v) [(k,Trie k v)]

data TrieCtx k v = Top
                 | Sub (Maybe v) k [(k,Trie k v)] (TrieCtx k v)

type TrieLoc  k v = (Trie k v, TrieCtx k v)
type StringTrie a = TrieCtx Char a
type StringLoc  a = TrieLoc Char a

-- ------------------------------------------------------
-- helper instances

instance (Show k, Show v) => Show (Trie k v) where
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

emptyTrie :: Eq k => Trie k v
emptyTrie  = Trie Nothing []

insertTrie :: Eq k => [k] -> v -> Trie k v -> Trie k v
insertTrie     [] e (Trie value children)
  = Trie (Just e) children
insertTrie (k:ks) e (Trie value children)
  = case lookup k children of
      Nothing ->
        Trie value (addEntry k (insertTrie ks e emptyTrie) children)
      Just subtree ->
        Trie value (addEntry k (insertTrie ks e subtree) children)

find :: Eq k => [k] -> Trie k v -> Maybe v
find [] (Trie value _)
  = value
find (k:ks) (Trie _ children)
  = case lookup k children of
      Nothing -> Nothing
      Just subtree -> find ks subtree

-- ------------------------------------------------------
-- zipper functions on tries

top :: Trie k v -> TrieLoc k v
top trie
  = (trie , Top)

down :: Eq k => k -> TrieLoc k v -> TrieLoc k v
down k (Trie value children , ctx)
  = case lookup k children of
      Nothing ->
        error "no path for key element"
      Just subtree ->
        (subtree , Sub value k (delEntry k children) ctx)


up :: TrieLoc k v -> TrieLoc k v
up (trie, Sub value key children ctx)
  = (Trie value ((key,trie):children), ctx)



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

