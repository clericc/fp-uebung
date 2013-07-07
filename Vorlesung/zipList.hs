data ListCtx a = Front | E (ListCtx a) a deriving Show

type Zipper a = ([a], ListCtx a)

forward :: Zipper a -> Zipper a
forward (x:xs, c) = (xs, E c x)

back :: Zipper a -> Zipper a
back (xs, E c x) = (x:xs, c)

first :: Zipper a -> Zipper a
first (l, Front) = (l, Front)
first z = first $ back z

selectElem :: Int -> Zipper a -> Zipper a
selectElem 0 x = x
selectElem n x = selectElem (n-1) (forward x)

front :: [a] -> Zipper a
front xs = (xs, Front)

toList :: Zipper a -> [a]
toList c = fst $ first c

l1 = forward $ front [1,2,3,4]
l2 = forward l1
l3 = forward l2
l4 = forward l3
l5 = back l4
l6 = back l5
l7 = back l6
l8 = back l7

