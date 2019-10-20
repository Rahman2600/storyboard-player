import qualified AesonDemo as Demo
import qualified SourceData as SD

--main = Demo.rundemo
--main = SD.test

-- Example of using the parser API:

-- example "callback" function for parser
-- simply checks whether the result of parsing is an error
-- error string or an SD.Story and prints
display :: Either String SD.Story -> IO()
display s =
    case s of
        Left err -> putStrLn err
        Right story -> print story

-- Call the parser with the above cb and example data
main = SD.parseStory "example_data/cpsc-312.json" display

-- Notes:
--
-- 1) The result will be an error if the file cannot be opened,
-- or if it does not match the expected Storyboard schema
-- 
-- 2) The SD.Story datatype is our intermediate data structure,
-- and an example story (in "Haskell record notation") can be
-- found in SampleParseOutput.txt
