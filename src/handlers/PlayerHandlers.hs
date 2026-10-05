{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module PlayerHandlers where

import Servant
import Data.Text (Text)
import Data.Int (Int32, Int64)
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)
import Hasql.Connection (Connection)
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
import PlayerDTO
import Players


getPlayerHandler :: Pool -> Int64 -> Handler [PlayerDTO]
getPlayerHandler pool gameId =  do
    res <- liftIO $ Players.findPlayer pool gameId
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right as -> return $ map Players.toPlayerDTO as

getPlayersHandler :: Pool -> Handler [PlayerDTO]
getPlayersHandler pool = do
    res <- liftIO $ Players.findPlayers pool
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right as -> return $ map Players.toPlayerDTO as

createPlayerHandler :: Pool -> PlayerDTO -> Handler NoContent
createPlayerHandler p pl = do
        res <- liftIO $ Players.insertPlayer pl p
        case res of
            Left _ -> throwError err500
            Right [] -> throwError err403
            Right _ -> return NoContent

deletePlayerHandler :: Pool -> Int64 -> Handler NoContent
deletePlayerHandler _ userId =
    if userId == 0
        then throwError err404
        else pure NoContent

updatePlayerHandler :: Pool -> Int64 -> PlayerDTO -> Handler NoContent
updatePlayerHandler _ userId _ =
    if userId == 0
        then throwError err404
        else pure NoContent  