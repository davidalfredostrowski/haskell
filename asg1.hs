--1.) Write a function called add_and_double which adds two numbers together and then 
--doubles the result.

addDouble :: Int -> Int -> Int
addDouble a b = (a + b) * 2

--ghci> addDouble 3 5
--16
--ghci> addDouble 10 20
--60
--ghci>


--2.) Write a function called solve_quadratic_equation which takes in three arguments
-- (a, b, and c) which are coefficients to the quadratic equation a x2 + b x + c = 0. 
--a, b, c, and x should have type Double. The output should be a tuple containing the 
--two roots. Don't worry about complex roots; if you apply the sqrt function to a 
--negative number you will get NaN (Not A Number). Use a let or a where expression to 
--define the square root of the discriminant (sqrt(b ** 2 - 4 * a * c)).

quad :: (Double, Double, Double) -> (Double, Double)
quad (a,b,c) = (d,e)
    where
        d = ((-b) + f)/ (a*2)
        e = ((-b) - f)/ (a*2)
        f = sqrt((b*b)-(4*a*c))
 

--3.) Write a function called first_n which takes an Int (not an Integer!) value (n) and returns a list of the first n Ints starting from 1.
--(note: you will be using the existing ‘take’ function from Haskell but note that this function will only start from the beginning of the list if the first value is 1 – your function will not return anything if your list does not contain a 1)
--Examples of the existing library’s take functionality: 
--Ghci> take 5 [-1,-2,-3,-4,-5]
--[-1,-2,-3,-4,-5]
--Ghci> take 5 [-1,-2,-3,-4,1,2,3,4,5]
--[-1,-2,-3,-4,1]
--However your function would return: 
--Ghci> my_take_from_one 5 [-1,-2,-3,-4,-5]
--[]
--Ghci> my_take_from_one  5 [-1,-2,-3,-4,1,2,3,4,5]
--[1,2,3,4,5]


-- In your solution, use an infinite list and the take function from the Prelude (http://zvon.org/other/haskell/Outputprelude/take_f.html). We use Ints rather than Integers because the take function's first argument is of type Int.





-- three
f_n :: Int -> [Int] -> [Int]
f_n a [] = []
f_n 0 b = []
f_n a (x:xs) = if x == 1 then (x : take (a-1 ) xs) else f_n a xs


--ghci> f_n 3 [9,9,9,1,1,2,3]
--[1,1,2]
--ghci> f_n 3 [10,10,1,9,9,9,1,1,2,3]
--[1,9,9]
--ghci>



--four 
--Re-write first_n to a new function, first_n_integers which will
-- take an Integer argument and return a list of Integers. Do this
-- by defining a local helper function, take_integer, which takes 
--an Integer as its first argument and a list of Integers as its 
--second argument. Use a let or a where expression to define the 
--local helper function. Note that you can use the error function 
--to signal an error. Check that take_integer's first argument is 
--     = 0 (a pattern guard works well) and that its second argument 
--is not an empty list if its first argument is greater than 0. -
--take_integer should be a recursive function.

myTake2 :: Integer -> [a] -> [a]
myTake2 0 _ = []
myTake2 _ [] = []
myTake2 n (x : xs)= x : myTake2 (n -1 ) xs



first_n_integers :: Integer -> [Integer] -> [Integer]
first_n_integers 0 _ = []
first_n_integers _ [] = []
first_n_integers n (x:xs) 
    | x == 1 = myTake n (x:xs) 
    | otherwise = first_n_integers n xs
    where
        myTake :: Integer -> [a] -> [a]
        myTake 0 _ = []
        myTake _ [] = []
        myTake n (x : xs)= x : myTake (n -1) xs


--ghci> first_n_integers 3 [5,4,3,7,9,8,1,2,3,4,5,6]
--[1,2,3]
--ghci> first_n_integers 3 [5,4,8,1,2,3,4,5,6]
--[1,2,3]
--ghci> first_n_integers 3 [5,4,8,1,9,8,8,2,3,4,5,6]
--[1,9,8]
--ghci>



double_factorial :: Integer -> Integer 
double_factorial 0 =1
double_factorial x = factorial(x) * double_factorial(x-1)
    where
        factorial :: Integer -> Integer
        factorial 0 =1
        factorial  n = n * factorial (n - 1)