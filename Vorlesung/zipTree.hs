
data Tree a = E | Node a (Tree a) (Tree a) deriving (Show)

data WayPoint a = LeftWay a (Tree a) 
                | RightWay a (Tree a)
                deriving (Show)
                
type Path a = [WayPoint a]

type ZipTree a = (Tree a, Path a)

left :: ZipTree a -> ZipTree a
left (Node x l r, ps) = (l, LeftWay x r:ps)

right :: ZipTree a -> ZipTree a
right (Node x l r, ps) = (r, LeftWay x l:ps)

up :: ZipTree a -> ZipTree a
up (n, LeftWay x r:ps) = (Node x n r, ps)
up (n, RightWay x l:ps) = (Node x l n, ps)

top :: Tree a -> ZipTree a
top t = (t, [])

data Ctx a = Top | L (Ctx a) (Tree a) a | R (Ctx a) (Tree a) a deriving (Show)

type CtxTree a = (Tree a, Ctx a)

ctxLeft :: CtxTree a -> CtxTree a
ctxLeft (Node x l r, c) = (l, L c r x)

ctxRight :: CtxTree a -> CtxTree a
ctxRight (Node x l r, c) = (r, R c l x)

aTree :: Tree Int
aTree = Node 0 (Node 1 (Node 2 E E) (Node 3 E E)) (Node 4 (Node 5 E E) (Node 6 E E))

changeTo8 :: Tree Int -> Tree Int
changeTo8 (Node a (Node b c (Node _ d e)) f)
      = (Node a (Node b c (Node 8 d e)) f)
