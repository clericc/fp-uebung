data BinTree a = Empty
               | Node (BinTree a) a (BinTree a)
               deriving (Show, Eq)

type FList a = [a] -> [a]

{-
binTreeToList  :: BinTree a -> [a]
binTreeToList Empty = [] 
binTreeToList (Node l x r) 
    = binTreeToList l
      ++
      [x]
      ++
      binTreeToList r
--}

-- converts a list into an Flist
fromList            :: [a] -> FList a
-- fromList l        = \ xs -> l ++ xs
-- fromList l xs     = l ++ xs
fromList             = (++)

-- converts an Flist into a list
toList              :: FList a -> [a]
-- toList f          =  f []
toList               = ($ [])


-- O(1)
empty                :: FList a
empty                 = id

-- O(1)
singleton            :: a -> FList a
-- singleton x        = \ xs -> x:xs
-- singleton x xs     = x:xs
singleton             = (:)
 
-- O(1)
cons                 :: a -> FList a -> FList a
-- cons x flist       = \ xs -> x : (flist xs)
-- cons x flist xs    = x : (flist xs)
-- cons x flist       = (x:) . flist
cons x                = (.) (x:)

-- O(1)
snoc                 :: FList a -> a -> FList a
-- snoc flist x       = \ xs -> flist (x:xs)
-- snoc flist x xs    = flist (x:xs)
snoc flist x          = flist . (x:)

-- O(1)
append               :: FList a -> FList a -> FList a   -- analog zu ++
-- append f1 f2       = \ xs -> f1 (f2 xs)
-- append f1 f2 xs    = f1 (f2 xs)
-- append f1 f2 xs    = f1 . f2 $ xs
-- append f1 f2       = f1 . f2
append                = (.)

-- convenience function
(+++) = append

-- O(n) ueber Anzahl der Elemente in allen FListen
concatF              :: [FList a] -> FList a
concatF []            = empty
concatF (x:xs)        = append x (concatF xs)

-- O(n) ueber Länge der FList
mapF                 :: (a -> b)  -> FList a -> FList b
mapF f                = foldrF (cons . f) empty

-- O(n) ueber Länge der FList
foldrF               :: (a -> b -> b) -> b -> FList a -> b
foldrF f e flist
  | nullF flist       = e
  | otherwise         = f (headF flist) (foldrF f e (tailF flist))


-- O(n) ueber Länge der FLis
foldlF               :: (a -> b -> a) -> a -> FList b -> a
foldlF f e flist
  | nullF flist       = e
  | otherwise         = foldlF f (f e (headF flist)) (tailF flist)
 
-- O(1)
headF                :: FList a -> a
headF                 = head . toList

-- O(1)
tailF                :: FList a -> FList a
-- tailF flist        = \ xs -> tail (flist xs)
-- tailF flist        = tail . flist
tailF                 =  (tail .)

-- O(1)
nullF           :: FList a -> Bool
nullF            = null . toList

-- O(n) ueber Länge der FList
reverseF        :: FList a -> FList a
reverseF         = foldrF (flip snoc) empty
-- reverseF      = foldlF (flip cons) empty


-- O(n) ueber Anzahl der Elemente im Baum
binTreeToFList :: BinTree a -> FList a
binTreeToFList Empty = empty
binTreeToFList (Node l x r)
  = \xs -> ((binTreeToFList l) 
           +++
           singleton x
           +++
           (binTreeToFList r))
           xs


binTreeToList   :: BinTree a -> [a]
binTreeToList   = toList . binTreeToFList
