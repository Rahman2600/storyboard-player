{-# LANGUAGE FlexibleInstances, OverloadedStrings#-}

module SourceData where

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

    decodeMetaEntry :: (String, Value) -> Parser SceneMeta
    decodeMetaEntry (key, val) = withObject "meta info" (\ o ->
            SceneMeta key <$> o .: "text") val

    instance {-# OVERLAPS #-} FromJSON [SceneMeta] where
        parseJSON x =
            parseJSON x >>= mapM decodeMetaEntry . toList


    -- Decode test code

    testJSON :: FilePath
    testJSON = "example_data/test.json"

    getJSON :: IO B.ByteString
    getJSON = B.readFile testJSON

    testJSONMeta :: FilePath
    testJSONMeta = "example_data/test_meta.json"

    getJSONMeta :: IO B.ByteString
    getJSONMeta = B.readFile testJSONMeta

    test :: IO ()
    test = do
        putStrLn "%================ Storyboard Parse Test =================%"
        putStrLn "(This should parse a single scene from file)"
        putStrLn "Decoding JSON from file..."
        content <- getJSON
        C.putStrLn content
        d <- (eitherDecode <$> getJSON) :: IO (Either String StoryScene)
        Prelude.putStrLn "Result:"
        case d of
            Left err -> putStrLn err
            Right ps -> print ps
        putStrLn "%--------------------------------------------------------%"
        putStrLn "(This should parse a list of scene metas from file)"
        putStrLn "Decoding..."
        content <- getJSONMeta
        C.putStrLn content
        d <- (eitherDecode <$> getJSONMeta) :: IO (Either String [SceneMeta])
        Prelude.putStrLn "Result:"
        case d of
            Left err -> putStrLn err
            Right ps -> print ps
