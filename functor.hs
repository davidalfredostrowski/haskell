-- http://www.cs.umd.edu/class/spring2019/cmsc388F/lectures/applicative-functors.html

import Data.Functor as Functor

inc :: [Int] -> [Int]
inc [] = []
inc (i:is) = i+1 : inc is


square :: [Int] -> [Int]
square [] = []
square (i:is) = i^2 : square is



-- ghci> :l functor.hs
-- [1 of 1] Compiling Main             ( functor.hs, interpreted )
-- Ok, one module loaded.
-- ghci> inc [1,2,3,4,5]
-- [2,3,4,5,6]
-- ghci> square [1,2,3,4,5]
-- [1,4,9,16,25]
-- ghci>



imap :: (Int -> Int) -> [Int] -> [Int]
imap f [] = []
imap f (i:is) = (f i) : imap f is



inc' :: [Int] -> [Int]
inc' = imap ((+) 1)

square' :: [Int] -> [Int]
square' = imap (\x -> x^2)

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (i:is) = (f i) : map2 f is



incP2 :: [Int] -> [Int]
incP2 = map2 ((+) 1)

squareP2 :: [Int] -> [Int]
squareP2 = map2 (\x -> x^2)


data Tree a = Leaf |  Bin a (Tree a) (Tree a) deriving (Eq, Show)

tinc :: Tree Int -> Tree Int
tinc Leaf = Leaf
tinc (Bin i l r) = Bin (i + 1) (tinc l) (tinc r)


tsquare :: Tree Int -> Tree Int
tsquare Leaf = Leaf
tsquare (Bin i l r) = Bin (i^2) (tsquare l) (tsquare r)

tmap :: (a -> b) -> Tree a -> Tree b
tmap f Leaf = Leaf
tmap f (Bin i l r) = Bin( f i ) (tmap f l ) (tmap f r)

-- class Functor m where
--     fmap :: (a -> b) -> m a -> m b 

-- instance Functor List where 
--     fmap f [] = []
--     fmap f (i:is) = (f i) : fmap f is


instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Bin i l r) = Bin ( f i ) (fmap f l) (fmap f r)



-- ghci> fmap id [1,2,3,4]
-- [1,2,3,4]
-- ghci> (fmap (\x -> x^2) . fmap (\x -> x + 1)) [1,2,3,4]
-- [4,9,16,25]

-- ghci> fmap ((\x -> x^2) . (\x -> x + 1)) [1,2,3,4]
-- [4,9,16,25]
-- ghci>


createTree :: Int -> Tree Int
createTree 0 = Leaf
createTree n = Bin n l r 
    where l = createTree ( n - 1)
          r = createTree ( n - 1)

tree :: Tree Int
tree = createTree 3


mappedTree1 :: Tree Int
mappedTree1 = tmap (+1) (tmap (*2) tree)

sumTree :: Tree Int -> Int
sumTree Leaf = 0 
sumTree (Bin i l r)  = i + (sumTree l ) + (sumTree r)
 

main  = do 
    print $ sumTree mappedTree1


-- ghci> :l functor.hs
-- [1 of 1] Compiling Main             ( functor.hs, interpreted )
-- Ok, one module loaded.
-- ghci> sumTree tree
-- 4
-- ghci> tree
-- Bin 2 (Bin 1 Leaf Leaf) (Bin 1 Leaf Leaf)
-- ghci> :l functor.hs
-- [1 of 1] Compiling Main             ( functor.hs, interpreted )
-- Ok, one module loaded.
-- ghci> tree
-- Bin 3 (Bin 2 (Bin 1 Leaf Leaf) (Bin 1 Leaf Leaf)) (Bin 2 (Bin 1 Leaf Leaf) (Bin 1 Leaf Leaf))
-- ghci> sumTree tree
-- 11
-- ghci> :l functor.hs
-- [1 of 1] Compiling Main             ( functor.hs, interpreted )
-- Ok, one module loaded.
-- ghci> sumTree tree
-- 26
-- ghci> tree
-- Bin 4 (Bin 3 (Bin 2 (Bin 1 Leaf Leaf) (Bin 1 Leaf Leaf)) (Bin 2 (Bin 1 Leaf Leaf) (Bin 1 Leaf Leaf))) (Bin 3 (Bin 2 (Bin 1 Leaf Leaf) (Bin 1 Leaf Leaf)) (Bin 2 (Bin 1 Leaf Leaf) (Bin 1 Leaf Leaf)))
-- ghci>




-- ghci> sumTree mappedTree1
-- 29
-- ghci>



--ghci> :l functor.hs
-- [1 of 1] Compiling Main             ( functor.hs, interpreted )
-- Ok, one module loaded.
-- ghci> sumTree tree
-- 11
-- ghci> sumTree mappedTree1
-- 29
-- ghci>multiply by two first! then add one!




mappedTree2 :: Tree Int
mappedTree2 = tmap ((+1) . (*2)) tree





mappedTree3 :: Tree Int
mappedTree3 = fmap ((+1) . (*2)) tree