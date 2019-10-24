module Play where

import System.IO

-- Node Title Story Options
data Node = Node String String [Edge] | MtNode deriving (Show, Eq) 

-- Edge Option EndNode
data Edge = Edge String Node deriving (Show, Eq)

storyEnd = Node "You Died" "Unfortunately, you keeled over and died after making this choice" []
exampleStory = Node "Street corner" "You arrive at a street corner and are forced to make a choice" 
                    [(Edge "Go North" storyEnd),(Edge "Go South" storyEnd)]

play :: Node -> IO Node
play (Node title desc options) =
    do
        putStrLn "---------------------"
        putStrLn title
        putStrLn "---------------------"
        putStrLn desc
        putStrLn "---------------------"
        if(options /= [])
         then do
            putStr (printOptions options)
            putStrLn "Select an option: "
            ans <- getLine
            let ans2 = read ans
            let nextNode = getNext ans2 options
            if(nextNode == MtNode) 
                then do play (Node title desc options)
                else play nextNode
         else return (Node title desc options)

printOptions :: [Edge] -> String
printOptions a = printOptions' a 1

printOptions' :: (Show a, Num a) => [Edge] -> a -> String
printOptions' [] _ = ""
printOptions' ((Edge option node) : r) n = (show n) ++ ". " ++ option ++ "\n" ++ printOptions' r (n+1)

getNext :: (Eq a, Num a) => a -> [Edge] -> Node
getNext n a = getNext' a n 1

getNext' :: (Eq a, Num a) => [Edge] -> a -> a -> Node
getNext' [] _ _ = MtNode
getNext' ((Edge option node) : r) choice n
    | n == choice = node
    | otherwise = getNext' r choice (n + 1)
