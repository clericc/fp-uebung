type Zipper2 a = ([a], [a])

forward2 :: Zipper a -> Maybe (Zipper a)
forward2 ([], _) = Nothing
forward2 (x:xs, ys) = Just (xs, x:ys)

backward2 :: Zipper a -> Maybe (Zipper a)
backward2 ([], _) = Nothing
backward2 (xs, x:ys) = Just (x:xs, ys)

selectElem2 :: Int -> [a] -> Maybe (Zipper a)
selectElem2 _ [] = Nothing
selectElem2 n x = selEl2 n (return (x, []))
  where
    selEl2 :: Int -> Maybe (Zipper a) -> Maybe (Zipper a)
    selEl2 _ Nothing = Nothing
    selEl2 0 (Just a) = return a
    selEl2 n a = selEl2 (n-1) (a >>= forward2)
    

type Zipper a = ([a], [a])

forward :: Zipper a -> Zipper a
forward (x:xs, ys) = (xs, x:ys)

backward :: Zipper a -> Zipper a
backward (xs, x:ys) = (x:xs, ys)

selectElem :: Int -> [a] -> Zipper a
selectElem n x = selEl n (x, [])
  where
    selEl :: Int -> Zipper a -> Zipper a
    selEl 0 a = a
    selEl n a = selEl (n-1) (forward a)
    
data ListCtx a = Front | E (ListCtx a) a deriving Show

type ZipCtx a = ([a], ListCtx a)

forwardC :: ZipCtx a -> ZipCtx a
forwardC (x:xs, c) = (xs, E c x)

backwardC :: ZipCtx a -> ZipCtx a
backwardC (xs, E c x) = (x:xs, c)

selectElemC :: Int -> ZipCtx a -> ZipCtx a
selectElemC 0 x = x
selectElemC n x = selectElemC (n-1) (forwardC x)

front :: [a] -> ZipCtx a
front xs = (xs, Front)
    

