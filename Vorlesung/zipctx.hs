
data Tree a = E 
    | Node a (Tree a) (Tree a) deriving (Show)

data Ctx a = Top 
    | L (Ctx a) a (Tree a)
    | R (Ctx a) a (Tree a) deriving (Show)

type Zipper a = (Tree a, Ctx a)

left :: Zipper a -> Zipper a
left (Node x l r, c) = (l, L c x r)

right :: Zipper a -> Zipper a
right (Node x l r, c) = (r, R c x l)

up :: Zipper a -> Zipper a
up (n, L c x r) = (Node x n r, c)
up (n, R c x l) = (Node x l n, c)

upmost :: Zipper a -> Zipper a
upmost (n, Top) = (n, Top)
upmost x = upmost (up x)

top :: Tree a -> Zipper a
top t = (t, Top)

modify :: (Tree a -> Tree a) -> Zipper a -> Zipper a
modify f (t,c) = (f t, c)

aTree :: Tree Int
aTree = Node 0 
          (Node 1 
            (Node 2 E E) 
            (Node 3 E E)) 
          (Node 4 
            (Node 5 E E) 
            (Node 6 E E))

changeTo8 :: Tree Int -> Tree Int
changeTo8 (Node a (Node b c (Node _ d e)) f)
      = (Node a (Node b c (Node 8 d e)) f)
      

changeTo8'' = modify (\_ -> Node 8 E E) $ right . left . top $ aTree
