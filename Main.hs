import qualified Converter
import qualified Play as Player

parseAndPlay :: String -> IO ()
parseAndPlay s =
    do
        n <- Converter.extractStoryNode s
        case n of
            Nothing -> putStrLn "Error in parse - will not play"
            Just node ->
                do
                    putStrLn "Playing story!"
                    played <- Player.play node
                    print played

main = 
    do
        putStrLn "-------------% STORYBOARD PLAYER %-------------"
        putStrLn "Enter the name of a file to play:"
        file <- getLine
        putStrLn ("Loading '" ++ file ++ "'")
        parseAndPlay file
