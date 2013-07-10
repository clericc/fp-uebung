module TrieZipper where

import Data.AssocList
import Data.Char

-- ------------------------------------------------------------------
-- syntactic domains

data Trie k v = Trie (Maybe v) [(k,Trie k v)]

data TrieCtx k v = Top
                 | Sub (Maybe v) k [(k,Trie k v)] (TrieCtx k v)
                   deriving Show

type TrieLoc  k v = (Trie k v, TrieCtx k v)
type StringTrie a = TrieCtx Char a
type StringLoc  a = TrieLoc Char a

-- ------------------------------------------------------------------
-- semantic string representations

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

{-
instance (Show k, Show v) => Show (TrieCtx k v) where
  show = snd . show' ""
  where
  show' indent (Top)
    = (indent ++ "  ", "Top")
  show' indent (Sub Nothing k )
--}

-- ------------------------------------------------------------------
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

-- ------------------------------------------------------------------
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

downMul :: Eq k => [k] -> TrieLoc k v -> TrieLoc k v
downMul [] t = t
downMul (x:xs) t = downMul xs (down x t)

up :: TrieLoc k v -> TrieLoc k v
up (trie, Sub value key children ctx)
  = (Trie value ((key,trie):children), ctx)

upmost :: TrieLoc k v -> TrieLoc k v
upmost (t, Top) = (t, Top)
upmost z = upmost $ up z

modify :: (Trie k v -> Trie k v) -> TrieLoc k v -> TrieLoc k v
modify f (t, c) = (f t, c)

modifyElem :: (v -> v) -> TrieLoc k v -> TrieLoc k v
modifyElem  = modify . liftTrie

liftTrie :: (v -> v) -> Trie k v -> Trie k v
liftTrie f (Trie value children) = Trie (value >>= Just . f) children

-- ------------------------------------------------------------------
-- test tries

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


trie2 = foldr (\ (k,v) -> (insertTrie k v .) ) id

  [("", "empty"),
   ("rose", "rot"),
   ("rodeo", "cowboy"),
   ("raser", "auto")
  ]
  $ emptyTrie
