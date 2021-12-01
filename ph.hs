import Data.List
import Data.Char
import qualified Data.Map as Map

phoneBook2 :: Map.Map String String
phoneBook2 = Map.fromList $
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]


#:l ph.hs
#ghci> let newBook = Map.insert "grace" "341-9021" phoneBook2
#ghci> let newBook = Map.insert "grace" "543-8888" phoneBook2
#ghci> let newBook = Map.insert "grace" "999-9999" phoneBook2
#ghci> Map.lookup "grace" newBook
#Just "999-9999"
#ghci> Map.lookup "grace" phoneBook2
#Nothing
#ghci>










