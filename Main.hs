import qualified Converter -- JSON to Story Node Converter
import qualified Play as Player -- Allows us to play a Story

parseAndPlay :: String -> IO ()
-- Consume a file path, extract the story structure, and play
-- the game if parsed successfully
parseAndPlay s =
    do
        n <- Converter.extractStoryNode s
        case n of
            Nothing -> putStrLn "Error in parse - will not play"
            Just node ->
                do
                    putStrLn "Playing story!"
                    played <- Player.play node
                    putStrLn "Thanks for playing!"

-- main procedure
-- ask user for a filepath and attempt to load and play the file
main = 
    do
        putStrLn "-------------% STORYBOARD PLAYER %-------------"
        putStrLn "Enter the name of a file to play:"
        file <- getLine
        putStrLn ("Loading '" ++ file ++ "'")
        parseAndPlay file
