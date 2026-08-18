{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module GameDTO where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Int (Int32, Int64)
import Data.Text (Text, pack, unpack)


-- Game

data GameDTO = GameDTO
    { court :: Text
    , local :: Text
    , visit :: Text
    , setLocal :: Int64
    , setVisit :: Int64
    , date :: Maybe Int64
    , id :: Maybe Int64
    } deriving (Eq, Show)
    

instance ToJSON GameDTO where
    toJSON GameDTO {..} = object [
            "court" .= court,
            "local" .= local,
            "visit" .= visit,
            "setlocal" .= setLocal,
            "setvisit" .= setVisit,
            "date" .= date,
            "id" .= id
        ]

instance FromJSON GameDTO where
    parseJSON (Object v) = GameDTO <$> 
        v .: "court" <*>
        v .: "local" <*>
        v .: "visit" <*>
        v .: "setlocal" <*>
        v .: "setvisit" <*>
        v .:? "date" <*>
        v .:? "id"
    parseJSON _ = fail "GameDTO expects an object"
