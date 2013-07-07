

list = [1..100]

modify :: Int -> (a -> a) -> [a] -> [a]
modify 0 f (x:xs) = (f x):xs
modify n f (x:xs) = x:(modify (n-1) f xs)

f :: Int -> Int
f = (+1)

t = modify 90 f $ modify 87 f $ modify 89 f $ modify 88 f list


