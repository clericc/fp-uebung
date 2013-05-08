-- Uebung 3 von Julian Wefers und Alexander Mills

import Test.HUnit

-- Aufgabe 1
-- reverse in O(n)
reverse' :: [a] -> [a]
reverse' = revhelp []
  where 
    revhelp acc [] = acc
    revhelp acc (y:ys) = revhelp (y:acc) ys 
    
-- TestFaelle
tests = test [ "test1" ~: "r [1,2,3]" ~: (reverse [1,2,3]) ~=? (reverse' [1,2,3]),
               "test2" ~: "r [[1,2],[45],[9,1,5]]" ~: (reverse [[1,2],[45],[9,1,5]]) ~=? (reverse' [[1,2],[45],[9,1,5]]),
               "test3" ~: "r 'The path of the righteous man is beset on all sides by the inequities of the selfish and the tyranny of evil men. Blessed is he who, in the name of charity and good will, shepherds the weak through the valley of darkness, for he is truly his brother's keeper and the finder of lost children. And I will strike down upon thee with great vengeance and furious anger those who would attempt to poison and destroy My brothers. And you will know My name is the Lord when I lay My vengeance upon thee.'" ~: (reverse "The path of the righteous man is beset on all sides by the inequities of the selfish and the tyranny of evil men. Blessed is he who, in the name of charity and good will, shepherds the weak through the valley of darkness, for he is truly his brother's keeper and the finder of lost children. And I will strike down upon thee with great vengeance and furious anger those who would attempt to poison and destroy My brothers. And you will know My name is the Lord when I lay My vengeance upon thee.") ~=? (reverse' "The path of the righteous man is beset on all sides by the inequities of the selfish and the tyranny of evil men. Blessed is he who, in the name of charity and good will, shepherds the weak through the valley of darkness, for he is truly his brother's keeper and the finder of lost children. And I will strike down upon thee with great vengeance and furious anger those who would attempt to poison and destroy My brothers. And you will know My name is the Lord when I lay My vengeance upon thee."),
               "test4" ~: "r [1..100]" ~: (reverse [1..100]) ~=? (reverse' [1..100]),
               "test5" ~: "r [x `mod` 3 == 0 | x <- [1..100]]" ~: (reverse [x `mod` 3 == 0 | x <- [1..100]]) ~=? (reverse' [x `mod` 3 == 0 | x <- [1..100]])]

-- "Shortcut" zum Testen
testIt = runTestTT tests

-- Loesung mit Fold
reverseFold :: [a] -> [a]
reverseFold = foldl (flip (:)) []

-- Aufgabe 2
-- gegeben sei folgende Funktion
cross :: [a] -> [b] -> [(a,b)]
cross l1 l2 = [ (x,y) | x <- l1, y <- l2 ]

{- cross [1,2] [3,4] = [(1,3), (1,4), (2,3), (2,4)] -}

-- Loesung ohne List comprehension
cross' :: [a] -> [b] -> [(a,b)]
cross' [] _c = []
cross' (x:xs) ys = map ((,) x) ys ++ cross' xs ys


-- Aufgabe 3

takeWhile'           :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []       = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []
 
	
dropWhile'           :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []       = []
dropWhile' p l@(x:xs)
  | p x       = dropWhile' p xs
  | otherwise = l

-- Zeigen Sie, dass fuer alle Praedikate p und alle Listen xs gilt
-- takeWhile p xs ++ dropWhile p xs = xs

-- Durch Vollständige Induktion
-- Induktionsverankerung
{-
  takeWhile p [] ++ dropWhile p []
  {- Def. takeWhile -}
= [] ++ dropWhile p []
  {- Def. dropWhile -}
= [] ++ []
= []
-}

-- Induktionsschritt
{-
  takeWhile p x:xs ++ dropWhile p x:xs 
  {- Def. takeWhile u. dropWhile -}

  {- Fallunterscheidung: fuer p x == True -}
= x : takeWhile p xs ++ dropWhile p xs
= x : (takeWhile p xs ++ dropWhile p xs)
  {- Induktionsverankerung takeWhile p xs ++ dropWhile p xs = xs -}
= x : xs
q.e.d. Teil 1

  {- Fallunterscheidung p x == False -}
= [] ++ x:xs
= x:xs
q.e.d. Teil 2
-}


-- span
span :: (a -> Bool) -> [a] -> ([a], [a])
span p xs = (takeWhile p xs, dropWhile p xs)

-- Loesung, die die Liste nur einmal abarbeitet
{- 

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' p xs = takedropWhile p [] xs
  where 
    takedropWhile p xs (y:ys)
      | p y = takedropWhile (xs ++ y) ys
      | otherwise = (xs, y:ys)
 --}

span1 :: (a -> Bool) -> [a] -> ([a],[a])
span1 _ [] = ([],[])
span1 p l@(x:xs)
  | p x = (x:restL,restR)
  | otherwise = ([],l)
  where
    (restL,restR) = span1 p xs