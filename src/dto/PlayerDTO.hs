{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module PlayerDTO where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Int (Int32, Int64)

data PlayerDTO = PlayerDTO 
    { gameId :: Int64
    , playerId :: Int64
    } deriving (Eq, Show)

instance ToJSON PlayerDTO where
    toJSON PlayerDTO {..} = object [
            "gameid" .= gameId,
            "playerid" .= playerId
        ]

instance FromJSON PlayerDTO where
    parseJSON (Object v) = PlayerDTO <$> 
        v .: "gameid" <*>
        v .: "playerid"
    parseJSON _ = fail "PlayerDTO expects an object"