module StoryboardAPI where

-- Provides functions for converter and player fucntionality
-- in Ghci

import qualified Converter -- JSON to Story Node Converter
import qualified Play as Player -- Allows us to play a Story

-- Convert source data to a Story Node structure
convert :: String -> IO (Maybe Player.Node)
convert fp = Converter.extractStoryNode fp

play :: IO (Maybe Player.Node) -> IO ()
-- Play a game from the result of a parsed file
play ion = 
    do
        n <- ion
        case n of
            Nothing -> putStrLn "Error in parse - will not play"
            Just node ->
                do
                    putStrLn "Playing story!"
                    played <- Player.play node
                    putStrLn "Thanks for playing!"
