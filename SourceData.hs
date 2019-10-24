{-# LANGUAGE FlexibleInstances, OverloadedStrings#-}

module SourceData where

    -- Contains functions for extracting intermediate data-structures
    -- from JSON story data.

    import Data.Aeson
    import Data.Aeson.Types
    import System.IO
    import qualified Data.ByteString.Lazy as B
    import qualified Data.ByteString.Lazy.Char8 as C
    import Data.HashMap.Strict

    -- These define the elementary data schemas as they appear
    -- in the JSON files. Relations (such as links, parentNode, meta) 
    -- should be converted to new data structure according to ids.

    data StoryPort = 
        -- Represents a choice at a given scene. links is a list of 
        -- ids of the dst scenes.
        StoryPort {     port_id             :: String
                ,       port_type           :: String
                ,       port_name           :: String
                ,       parentNode          :: String
                ,       links               :: [String]
                ,       port_in             :: Bool
                ,       label               :: String
        } deriving (Show)

    data StoryScene =
         -- Represents a scene with a list of ports. scene_name is
         -- the primary text which appears above the scene page in the
         -- web view. Full body text is contained in the meta
        StoryScene {    scene_id            :: String
                ,       scene_type          :: String
                ,       scene_ports         :: [StoryPort]
                ,       scene_name          :: String
        } deriving (Show)

    data SceneMeta =
        -- Additional scene information, such as text
        -- link_id is the corresponding scene's id
        SceneMeta {     link_id             :: String
                ,       text                :: String
        } deriving (Show)

    data StoryScenes =
        -- Contains story scenes
        StoryInfo {     items               :: [StoryScene]
        } deriving (Show)

    data Story =
        -- Top-level story abstraction
        Story {     story_slug              :: String
                ,   story_scenes            :: StoryScenes
                ,   scene_meta              :: [SceneMeta]
        } deriving (Show)


    -- object translation from JSON. keys refer to the original keys in 
    -- the file.

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

    decodeMetaEntry :: (String, Value) -> Parser SceneMeta
    -- Decodes the "meta" component of the file
    decodeMetaEntry (key, val) = withObject "meta info" (\ o ->
            SceneMeta key <$> o .: "text") val

    instance {-# OVERLAPS #-} FromJSON [SceneMeta] where
        parseJSON x =
            parseJSON x >>= mapM decodeMetaEntry . toList

    -- Selective decoding of the source file. We only use the slug, nodes,
    -- and meta fields

    instance FromJSON StoryScenes where
        parseJSON = withObject "story" $ \o ->
            StoryInfo   <$> o .: "nodes"

    instance FromJSON Story where
        parseJSON = withObject "source" $ \o ->
            Story       <$> o .: "slug"
                        <*> o .: "story"
                        <*> o .: "meta"


    -- Decode test code

    testJSON :: FilePath
    testJSON = "example_data/test.json"

    getJSON :: IO B.ByteString
    getJSON = B.readFile testJSON

    testJSONMeta :: FilePath
    testJSONMeta = "example_data/test_meta.json"

    getJSONMeta :: IO B.ByteString
    getJSONMeta = B.readFile testJSONMeta

    testJSONGame :: FilePath
    testJSONGame = "example_data/cpsc-312.json"

    getJSONGame :: IO B.ByteString
    getJSONGame = B.readFile testJSONGame

    test :: IO ()
    test = do
        putStrLn "%================ Storyboard Parse Test =================%"
        putStrLn "1. (This should parse a single scene from file)"
        putStrLn "Decoding JSON from file..."
        content <- getJSON
        C.putStrLn content
        d <- (eitherDecode <$> getJSON) :: IO (Either String StoryScene)
        Prelude.putStrLn "Result:"
        case d of
            Left err -> putStrLn err
            Right ps -> print ps
        putStrLn "%--------------------------------------------------------%"
        putStrLn "2. (This should parse a list of scene metas from file)"
        putStrLn "Decoding..."
        content <- getJSONMeta
        C.putStrLn content
        d <- (eitherDecode <$> getJSONMeta) :: IO (Either String [SceneMeta])
        Prelude.putStrLn "Result:"
        case d of
            Left err -> putStrLn err
            Right ps -> print ps
        putStrLn "%--------------------------------------------------------%"
        putStrLn "3. (This should parse full game info from file)"
        putStrLn "Decoding..."
        d <- (eitherDecode <$> getJSONGame) :: IO (Either String Story)
        Prelude.putStrLn "Result:"
        case d of
            Left err -> putStrLn err
            Right ps -> print ps

    -- API

    parseStory :: FilePath -> IO (Either String Story)
    -- parse story consumes a path to a JSON file and outputs
    -- the result of parsing, which will be either an error string
    -- or Story object
    parseStory fp = let getJSON = B.readFile fp in
        (eitherDecode <$> getJSON)
