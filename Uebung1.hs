-- Übung 1

--Aufgabe 1
--Typ von h
f       :: Integer -> Integer
g       :: Integer -> (Integer -> Integer)
 
h       :: Integer -> Integer -> Integer
h x y   =  f (g x y)


-- Welche sind korrekt?
h       = f . g          korrekt
h x     = f . (g x)      korrekt
h x y   = (f . g) x y    korrekt

-- Definieren sie eine Fkt uncurry

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y)  = f x y

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

--Zeigen sie, dass die folgenden Eigenschaften gelten

-- 1.
  curry (uncurry f) x y
    {- Definition von curry -}
= uncurry f (x,y)
    {- Definition von uncurry -}
= f x y

-- 2.
  uncurry (curry f) (x, y)
  {- Definition von uncurry -}
= curry f x y
  {- Definition von curry -}
= f (x, y)

-- Aufgabe 3

-- gegeben:
(f >>> g) x   =  (g . f) x
 
(f &&& g) x   = (f x, g x)
 
(f *** g)     = (fst >>> f) &&& (snd >>> g)


-- Was macht denn eigentlich (***)?
  (+1) *** (+2) (1,2)
= (fst >>> (+1)) &&& (snd >>> (+2)) (1,2)
= (fst >>> (+1) (1,2), snd >>> (+2) (1,2))
= (2, 4)



-- Welche Typen besitzten >>>, &&& und ***?
(>>>) :: (a -> b) -> (b -> c) -> a -> c

(&&&) :: (a -> b) -> (a -> c) -> a -> (b,c)

(***) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)


-- Gilt das folgende Gesetz?
(f &&& g) >>> fst = f

    {- Argumentation mit Argument x -}
  (f &&& g) >>> fst x
    {- (>>>) -}
= fst . (f &&& g) x
    {- (.):    (f . g) x = f (g x) -}
= fst ((f &&& g) x)
    {- (&&&) -}
= fst (f x, g x)
    {- fst -}
= f x
    {- q.e.d -}


-- Gilt das folgende Gesetz?
(f &&& g) >>> snd = g

  (f &&& g) >>> snd x
    {- (>>>) -}
= snd . (f &&& g) x
    {- (.) -}
= snd ((f &&& g) x)
    {- (&&&) -}
= snd (f x, g x)
    {- snd -}
= g x
    {- q.e.d -}


-- Gilt das folgende Gesetz? 
h >>> (f &&& g) = (h >>> f) &&& (h >>> g)

  h >>> (f &&& g) x
    {- (>>>) -}
= (f &&& g) . h x
    {- (.) -}
= (f &&& g) (h x)
    {- (&&&) -}
= (f (h x), g (h x))
    {- (>>>) ^-1 -}
= (h >>> f x, h >>> g x)
    {- (&&&) ^-1 -}
= (h >>> f) &&& (h >>> g) x


-- Aufgabe 4
collatz :: (Integral a, Integral b) => a -> b
collatz x
  | x == 1 = 0
  | even x = 1 + collatz (x `div` 2)
  | otherwise = 1 + collatz (3 * x + 1)

collatz1 :: (Integral a, Integral b) => a -> b
collatz1 = collatz1' 0
  where collatz1' count x
               | x == 1 = count
               | even x = collatz1' (count+1) (x `div` 2)
               | otherwise = collatz1' (count+1) (3 * x + 1)

imax :: (Integral a, Integral b) => (a -> b) -> a -> a -> b
imax f lower upper = foldl max 0 . map f $ [lower..upper]

cmax :: (Integral a, Integral b) => a -> a -> b
cmax = imax collatz1

imax2 :: (Integral a, Integral b, Integral c) => (a -> b) -> a -> a -> (c, b)
imax2 f lower upper = foldl (\acc e -> if snd e > snd acc then e else acc) (0,0) . mapWithPos 0 f $ [lower..upper]
  where
  mapWithPos pos f [x] = [(pos, f x)]
  mapWithPos pos f (x:xs) = (pos, f x):(mapWithPos (pos+1) f xs)
