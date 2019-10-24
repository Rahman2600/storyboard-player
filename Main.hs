import qualified Converter
import qualified Play as Player

parseAndPlay :: String -> IO ()
parseAndPlay s =
    do
        n <- Converter.extractStoryNode s
        case n of
            Nothing -> putStr "Error in parse - will not play"
            Just node ->
                do
                    played <- Player.play node
                    print played

main = parseAndPlay "example_data/cpsc-312-acyclic.json"
