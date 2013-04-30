Aufgabe 1: Zeigen Sie durch Induktion über die Länge einer Liste, dass für alle Listen xs gilt:

  reverse (reverse xs) = xs
 
für reverse definiert als
 
reverse :: [a] -> [a]
reverse []       = []
reverse (x : xs) = reverse xs ++ [x]


Verankerung
  reverse (reverse [x])
    {- Def. reverse innen -}
= reverse (reverse [] ++ [x])
    {- Def. reverse -}
= reverse ([] ++ [x])
= reverse [x]
	{- Def. reverse -}
= reverse [] ++ [x]
= [] ++ [x]
= [x]

Wir nehmen an, dass "reverse (reverse xs) = xs" gilt, wir folgern also x:xs (als n+1. schritt)
  reverse (reverse (x:xs))
= reverse (reverse xs ++ [x])
= reverse (reverse y:ys ++ [x])
= reverse (reverse ys ++ [y] ++ [x])

= reverse (x : reverse xs)
    {- Def. reverse -}
= reverse (reverse xs) ++ [x]
    {- Einsetzen von "reverse (reverse xs) == xs" -}
= xs ++ [x]



--------- AUFGABE 2

Zeigen Sie, dass für alle Listen xs und ys gilt:
length (xs ++ ys) = length xs + length ys

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

Verankerung mit [x] und [y]

  length ([x] ++ [y])
= length ([x,y])
= 1 + length [y]
= 1 + 1 + length []
= 1 + 1 + 0
= 2

= length [x] + length [y]
= 1 + length [] + 1 + length []
= 1 + 0 + 1 + 0
= 2

Induktion für (x:xs) und (y:ys) mit length (xs ++ ys) == length xs + length ys
= length (x:xs ++ y:ys)
= 1 + length



------- AUFGABE 3

Eine duale Sicht auf Listen ist die, Elemente an das Ende einer Liste anzuhängen. Folgender Datentyp ist dafür geeignet (Snoc = Cons rückwärts gelesen).
 
> data RList a = Nil | Snoc (RList a) a deriving (Show, Eq)
 
Schreiben Sie Konversionsfunktionen, die normale Listen in diese Struktur überführen und umgekehrt und die elementaren Zugriffsfunktionen rHead, rTail, rInit, rLast:
 
> listToRList :: [a] -> RList a
> listToRList = foldl (\rlist x -> Snoc rlist x) Nil

Laufzeit linear

> rlistToList     :: RList a -> [a]
> rlistToList     = rlistToList' []
>   where
>   rlistToList' xs Nil = xs
>   rlistToList' xs (Snoc ys y) = rlistToList' (y:xs) ys

Laufzeit linear

> rHead              :: RList a -> a
> rHead Nil          = error "empty RList"
> rHead (Snoc Nil x) = x
> rHead (Snoc xs x)  = rHead xs

Laufzeit linear

> rTail             :: RList a -> RList a
> rTail Nil          = error "empty RList"
> rTail (Snoc Nil x) = Nil
> rTail (Snoc xs x)  = Snoc (rTail xs) x 

Laufzeit linear

> rInit            :: RList a -> RList a
> rInit Nil         = error "empty RList"
> rInit (Snoc xs _) = xs

Laufzeit konstant


> rLast           :: RList a -> a
> rLast Nil = error "empty RList"
> rLast (Snoc _ x) = x

Laufzeit konstant


------- Aufgabe 4

sei folgendermaßen definiert:
 
splitAt         :: Int -> [a] -> ([a], [a])
splitAt n xs    = (take n xs, drop n xs)

Entwickeln Sie eine Implementierung, die die beiden Teillisten mit einem Durchlauf durch die Argumentliste direkt berechnet.

> splitAt1 :: Int -> [a] -> ([a], [a]) 
> splitAt1 = splitAt1' ([],[])
>   where
>   splitAt1' (xs,ys) _ ([]) = (xs,ys)
>   splitAt1' (xs,ys) cnt l@(z:zs)
>     | cnt > 0   = splitAt1' (xs ++ [z],ys) (cnt-1) zs
>     | otherwise = (xs,l)
