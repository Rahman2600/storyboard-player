{--
Haskell JSON parse demo using Aeson
From 
https://www.schoolofhaskell.com/school/starting-with-haskell/
libraries-and-frameworks/text-manipulation/json
--}

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module AesonDemo where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics


data Person =
    Person { firstName  :: !Text
           , lastName   :: !Text
           , age        :: Int
           , likesPizza :: Bool
    } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person


jsonURL :: String
jsonURL = "http://daniel-diaz.github.io/misc/pizza.json"


getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL


rundemo :: IO ()
rundemo = do
    Prelude.putStrLn "%================ Aeson Parse Demo =================%"
    Prelude.putStrLn "Decoding JSON from remote..."
    -- Get JSON data and decode it
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Person])
    -- If d is Left, the JSON was malformed.
    -- In that case, we report the error.
    -- Otherwise, we perform the operation of
    -- our choice. In this case, just print it.
    Prelude.putStrLn "Result:"
    case d of
        Left err -> putStrLn err
        Right ps -> print ps
