{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Players (findPlayer, findPlayers, insertPlayer, toPlayerDTO) where

import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
--import qualified Data.Text.Internal as TI
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release)
import qualified Hasql.Session as Session
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
import Rel8
import Prelude hiding (filter, null)

import PlayerDTO

data Player f = Player
    { playerId :: Column f Int64
    , gameId :: Column f Int64
    } deriving stock (Generic)
      deriving anyclass (Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Player f)

playerSchema :: TableSchema (Player Name)
playerSchema = TableSchema
    { name = "players"
    , columns = Player
        { gameId = "game_id"
        , playerId = "player_id"
        }
    }


findPlayers :: Pool -> IO (Either P.UsageError [Player Result])
findPlayers pool = do
    let query = select $ each playerSchema
    P.use pool (Session.statement () (run query))

findPlayer :: Pool -> Int64 -> IO (Either P.UsageError [Player Result])
findPlayer pool gameId = do
    let query = select $ do
            p <- each playerSchema
            where_ (p.gameId ==. lit gameId)
            return p
    P.use pool (Session.statement () (run query))


-- INSERT
insertPlayer :: PlayerDTO -> Pool -> IO (Either P.UsageError [Int64])
insertPlayer p pool = do
    P.use pool (Session.statement () (run (insert1 p)))

insert1 :: PlayerDTO -> Statement (Query (Expr Int64))
insert1 p = insert $ Insert
            { into = playerSchema
            , rows = values [ Player (lit p.playerId) (lit p.gameId) ]
            , returning = Returning toPlayerId
            , onConflict = Abort
            }

-- Mapper
toPlayerDTO :: Player Rel8.Result -> PlayerDTO
toPlayerDTO player = PlayerDTO
    { gameId = player.gameId
    , playerId = player.playerId
    }

toPlayerId :: Player Expr -> Expr Int64
toPlayerId player = player.gameId