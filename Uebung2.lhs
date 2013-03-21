Aufgabe 1: Zeigen Sie durch Induktion über die Länge einer Liste, dass für alle Listen xs gilt:

  reverse (reverse xs) = xs
 
für reverse definiert als
 
reverse :: [a] -> [a]
reverse []       = []
reverse (x : xs) = reverse xs ++ [x]


Verankerung
  reverse (reverse [x])
    {- Def. reverse innen -}
= reverse (reverse ([] ++ [x]))
    {- Def. reverse -}
= reverse ([x] ++ reverse [])
= reverse ([x] ++ [])
= reverse [] ++ [x]
= [] ++ [x]
= [x]

Wir nehmen an, dass "reverse (reverse xs) = xs" gilt, wir folgern also x:xs (als n+1. schritt)
  reverse (reverse (x:xs))
  {- Def. reverse innen -}
= reverse (reverse xs ++ [x])
  {- Lemma: reverse (xs ++ [x]) = x : reverse xs -}
= x : reverse . reverse xs
  {- reverse . reverse xs = xs gilt (siehe Verankerung -}
= x : xs

=> Jetzt müssen wir noch zeigen dass "reverse (xs ++ [x]) = x : reverse xs gilt"
Verankerung:
  reverse ([] ++ [x])
= reverse ([x])
= x

Induktion. Wir nehmen an, dass reverse (xs ++ [x]) = x:reverse xs gilt
Induktionsschritt:
  reverse (y:xs ++ [x])
  {- Def. ++ -}
= reverse (y:(xs ++ [x]))
  {- Def. reverse -}
= reverse (xs ++ [x]) ++ [y]
  {- Induktionssatz -}
= x : reverse (xs) ++ [y]
  {- Def. reverse -}
= x : reverse (y:xs)
q.e.d.



--------- AUFGABE 2

Zeigen Sie, dass für alle Listen xs und ys gilt:
length (xs ++ ys) = length xs + length ys

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

Verankerung mit [x] ++ [y]

  length ([x] ++ [y])
= 1 + length ([y])
= length [x] + length [y]

Induktionssatz: Wir nehmen an, dass "length (xs ++ ys) = length xs + length ys " gilt. Nun für "length (x:xs ++ ys)"
  length (x:xs ++ ys)
= length (x:(xs ++ ys))
  {- Def. length -}
= 1 + length (xs ++ ys)
  {- Einsetzen der Verankerung -}
= 1 + length xs + length ys
  {- length x entspricht 1, also kann hier length x eingesetzt werden -}
= length x + length xs + length ys
  {- Def. length -}
= length (x:xs) + length ys
q.e.d.




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
