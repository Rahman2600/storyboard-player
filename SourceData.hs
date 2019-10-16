{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module SourceData where

    import Data.Aeson
    import System.IO
    import qualified Data.ByteString.Lazy as B
    import qualified Data.ByteString.Lazy.Char8 as C

    -- These define the elementary data schemas as they appear
    -- in the JSON files. Relations (such as links, parentNode, meta) 
    -- should be converted to new data structure according to ids.

    data StoryPort = 
        -- Represents a choice at a given scene. links is a list of 
        -- ids of the dst scenes.
        StoryPort { port_id             :: String
                ,   port_type           :: String
                ,   port_name           :: String
                ,   parentNode          :: String
                ,   links               :: [String]
                ,   port_in             :: Bool
                ,   label               :: String
        } deriving (Show)

    data StoryScene =
         -- Represents a scene with a list of ports. scene_name is
         -- the primary text which appears above the scene page in the
         -- web view. Full body text is contained in the meta
        StoryScene { scene_id           :: String
                ,   scene_type          :: String
                ,   scene_ports         :: [StoryPort]
                ,   scene_name          :: String
        } deriving (Show)


    -- object translation from JSON. keys refer to the original keys in 
    -- the file.
    -- TODO figure out why the arrays give errors

    instance FromJSON StoryPort where
        parseJSON = withObject "story port" $ \o ->
            StoryPort   <$> o .: "id"
                        <*> o .: "type"
                        <*> o .: "name"
                        <*> o .: "parentNode"
                        <*> o .: "links"
                        <*> o .: "in"
                        <*> o .: "label"

    instance FromJSON StoryScene where
        parseJSON = withObject "story scene" $ \o ->
            StoryScene  <$> o .: "id"
                        <*> o .: "type"
                        <*> o .: "ports"
                        <*> o .: "name"


    -- Decode test code

    data Test =
        Test {
            strings :: [String]
        } deriving (Show)

    instance FromJSON Test where
        parseJSON = withObject "test obj" $ \o ->
            Test    <$> o .: "strings"

    testJSON :: FilePath
    testJSON = "example_data/test.json"

    getJSON :: IO B.ByteString
    getJSON = B.readFile testJSON

    test :: IO ()
    test = do
        putStrLn "%================ Storyboard Parse Demo =================%"
        putStrLn "(This should parse a single object from file)"
        putStrLn "Decoding JSON from file..."
        content <- getJSON
        C.putStrLn content
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Test])
        Prelude.putStrLn "Result:"
        case d of
            Left err -> putStrLn err
            Right ps -> print ps
