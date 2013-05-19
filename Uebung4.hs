data BinTree a = Empty
               | Node (BinTree a) a (BinTree a)

type FList a = [a] -> [a]


binTreeToList  :: BinTree a -> [a]
binTreeToList Empty = [] 
binTreeToList (Node l x r) 
    = binTreeToList l
      ++
      [x]
      ++
      binTreeToList r

-- converts a list into an Flist
fromList        :: [a] -> FList a
fromList l      = \ xs -> l ++ xs

-- converts an Flist into a list
toList          :: FList a -> [a]
toList l        = l []


-- to be implemented
empty           :: FList a
empty            = id

singleton       :: a -> FList a
singleton x      = \ xs -> x:xs
 
cons            ::       a -> FList a -> FList a
cons x flist     = \ xs -> x : (flist xs)

snoc            :: FList a ->       a -> FList a
snoc flist x     = \ xs -> flist (x:xs)

append          :: FList a -> FList a -> FList a   -- analog zu ++
append xs ys     = \ zs -> xs (ys zs)

concatF         :: [FList a] -> FList a
concatF []       = empty
concatF (x:xs)   = \ ys -> append x (concatF xs) ys

mapF            :: (a -> b)  -> FList a -> FList b
mapF             = undefined

foldrF          :: (a -> b -> b) -> b -> FList a -> b
foldrF f e flist
  | nullF flist = e
  | otherwise  = undefined
 
headF           :: FList a -> a
headF flist
  | nullF flist = error "empty Flist"
  | otherwise   = head . toList $ flist

tailF           :: FList a -> FList a
tailF flist      = undefined

nullF           :: FList a -> Bool
nullF flist      = null (flist [])

reverseF        :: FList a -> FList a
reverseF = undefined
