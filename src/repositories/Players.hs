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
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
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
    , schema = Nothing
    , columns = Player
        { gameId = "game_id"
        , playerId = "player_id"
        }
    }


findPlayers :: Pool -> IO (Either P.UsageError [Player Result])
findPlayers pool = do
    let query = select $ do
                    each playerSchema
    P.use pool (statement () query)

findPlayer :: Pool -> Int64 -> IO (Either P.UsageError [Player Result])
findPlayer pool playerId = do
                            let query = select $ do
                                            p <- each playerSchema
                                            where_ (p.playerId ==. lit playerId)
                                            return p
                            P.use pool (statement () query)


-- INSERT
insertPlayer :: PlayerDTO -> Pool -> IO (Either P.UsageError [Int64])
insertPlayer p pool = do
                            P.use pool (statement () (insert1 p))

insert1 :: PlayerDTO -> Statement () [Int64]
insert1 p = insert $ Insert
            { into = playerSchema
            , rows = values [ Player (lit p.gameId) (lit p.playerId) ]
            , returning = Projection (.gameId)
            , onConflict = Abort
            }

-- Mapper
toPlayerDTO :: Player Rel8.Result -> PlayerDTO
toPlayerDTO player = PlayerDTO
    { gameId = player.gameId
    , playerId = player.playerId
    }
