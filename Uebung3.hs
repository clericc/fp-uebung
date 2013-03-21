-- Uebung 3 von Julian Wefers und Alexander Mills

import Test.HUnit

-- Aufgabe 1
-- reverse in O(n)
reverse' :: [a] -> [a]
reverse' xs = revhelp xs []
  where 
    revhelp :: [a] -> [a] -> [a]
    revhelp [] l = l
    revhelp (y:ys) l = revhelp ys (y:l)
    
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
reverseFold l = foldl (flip (:)) [] l
