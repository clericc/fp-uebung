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

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (t:ts, c) = ((f t):ts, c)


modifyN :: Int -> (a -> a) -> Zipper a -> Zipper a
modifyN 0 f (l:ls, c) = ((f l):ls, c)
modifyN n f z 
    | n>0 = modifyN (n-1) f (forward z)
    | n<0 = modifyN (n+1) f (back z)


doSomething = modify (+1) . modify (*2)
doSomething2 = modifyN 0 (+1) . modifyN 2 (*2)
doSomething3 = modifyN 0 (+1) . modifyN 1 (*2) . modifyN 0 (+1) . modifyN 9999 (*2)

l1 = doSomething $ forward $ forward $ front [1,2,3]
l2 = doSomething2 $ front [1,2,3]
l3 = doSomething3 $ front [1..10005]

