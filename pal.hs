respondPalindrones :: String -> String
respondPalindrones = 
    unlines . 
    map (\xs -> if isPal xs then "palindrone" else "not a palidrone") .
    lines

isPal :: String -> Bool
isPal xs = xs == reverse xs