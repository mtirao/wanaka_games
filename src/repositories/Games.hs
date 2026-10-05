{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Games (findGame, findGames, insertGame, toGameDTO) where

import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release)
import qualified Hasql.Session as Session
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
import Rel8
import Prelude hiding (filter, null)

import GameDTO

data Game f = Game
    { court :: Column f Text
    , local :: Column f Text
    , visit :: Column f Text
    , setLocal :: Column f Int64
    , setVisit :: Column f Int64
    , date :: Column f Int64
    , gameId :: Column f Int64
    } deriving stock (Generic)
      deriving anyclass (Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Game f)

gameSchema :: TableSchema (Game Name)
gameSchema = TableSchema
    { name = "games"
    , columns = Game
        { court = "court"
        , local = "local"
        , visit = "visit"
        , setLocal = "set_local"
        , setVisit = "set_visit"
        , date = "date"
        , gameId = "id"
        }
    }


findGames :: Pool -> IO (Either P.UsageError [Game Result])
findGames pool = do
    let query = select $ do
                    each gameSchema
    P.use pool (Session.statement () (run query))

findGame :: Pool -> Int64 -> IO (Either P.UsageError [Game Result])
findGame pool gameId = do
                            let query = select $ do
                                            p <- each gameSchema
                                            where_ (p.gameId ==. lit gameId)
                                            return p
                            P.use pool (Session.statement () (run query))


-- INSERT
insertGame :: GameDTO -> Pool -> IO (Either P.UsageError [Int64])
insertGame p pool = do
    P.use pool (Session.statement () (run (insert1 p)))

insert1 :: GameDTO -> Statement (Query (Expr Int64))
insert1 p = insert $ Insert
            { into = gameSchema
            , rows = values [ Game (lit p.court) (lit p.local) (lit p.visit) (lit p.setLocal) (lit p.setVisit) (lit $ fromMaybe 0 p.date) (nextval "game_id_seq") ]
            , returning = Returning (.gameId)
            , onConflict = Abort
            }

-- Mapper
toGameDTO :: Game Rel8.Result -> GameDTO
toGameDTO game = GameDTO
    { court = game.court
    , local = game.local
    , visit = game.visit
    , setLocal = game.setLocal
    , setVisit = game.setVisit
    , date = Just game.date
    , id = Just game.gameId
    }
