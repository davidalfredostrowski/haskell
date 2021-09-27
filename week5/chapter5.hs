multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z


multThree2 :: Int -> (Int -> (Int -> Int))
multThree2 x y z = x * y * z


compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x


divideByTen :: (Floating a) => a -> a
divideByTen = (/10)


isUpperAlphanum :: Char -> Bool
isUpperAlphanum = ( `elem` ['A'..'Z'])


--ghci> isUpperAlphanum 'a'
--False
--ghci> isUpperAlphanum 'A'
--True
--ghci>

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f ( f x )

--ghci> applyTwice (++ " HAHA") "HEY"
--"HEY HAHA HAHA"
--ghci>


zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


flip' :: (a->b->c) -> (b ->a ->c)
flip' f = g 
    where g x y = f y x

--ghci> flip'  (-) 5 3
--  -2
--ghci> flip'  (-) 3 5
--   2
--ghci>

--ghci> flip (++) "there" "hello"
--"hellothere"
--ghci> flip (++) "1 " "2 "
--"2 1 "
--ghci> flip (-) 6 2
---4
--ghci> flip (-) 2 6
--4
--ghci>



flip2 :: (a->b->c) -> (b ->a ->c)
flip2   f y x = f x y


map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (x:xs) = f x : map2 f xs




--ghci> map2 (+3) [1,4,3,2,6]
--[4,7,6,5,9]
--ghci> map (++ "!") ["BIFF", "BANG", "POW"]
--["BIFF!","BANG!","POW!"]
--ghci> map (replicate 3) [3..6]
--[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
--ghci> map (map (^2)) [[1,2],[3,4,5,6], [7,8]]
--[[1,4],[9,16,25,36],[49,64]]
--ghci> map2 (map2 (^2)) [[1,2],[3,4,5,6], [7,8]]
--[[1,4],[9,16,25,36],[49,64]]
--ghci> map2 fst [(1,2),(3,5),(6,3),(2,5)]
--[1,3,6,2]
--ghci>




filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 p (x:xs)
    | p x  = x : filter2 p xs
    | otherwise = filter2 p xs



--ghci> filter ( `elem` ['a'..'z'] ) " u Laugh at ME beCause I am differEnt"
--"uaughatbeauseamdiffernt"
--ghci> filter ( `elem` ['A'..'Z'] ) " u Laugh at ME beCause I am differEnt"
--"LMECIE"
--ghci> filter (<15) (filter even [1..20])
--[2,4,6,8,10,12,14]
--ghci> [ x | x <- [1..20], x < 15, even x]
--[2,4,6,8,10,12,14]
--ghci>


quickS :: (Ord a) => [a] -> [a]
quickS [] = []
quickS (x:xs) = 
    let smallerOrEqual = filter (<= x) xs
        larger = filter (> x) xs
    in quickS smallerOrEqual ++ [x] ++ quickS larger


--ghci> quickS [1,4,2,5,4,7]
--[1,2,4,4,5,7]


largestDivisible :: Integer
largestDivisible = head (filter  p [99999,99998..])
    where p x = x `mod` 3829 == 0

--ghci> largestDivisible
--99554



--ghci> takeWhile (/=' ') "elephants know how to part"
--"elephants"

--ghci> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
--166650

--ghci> sum (takeWhile (<10000) [m | m <- [n^2 | n <- [1..]], odd m])
--166650



chain2 :: Integer -> [Integer]
chain2 1 = [1]
chain2 n
    | even n = n:chain2 (n `div` 2)
    | odd n  = n:chain2 (n*3 + 3)

numLongChains :: Int
numLongChains = length (filter isLong (map chain2 [1..100]))
    where isLong xs = length xs > 15
