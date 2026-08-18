{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module GameHandlers where

import Servant
import Data.Text (Text)
import Data.Int (Int32, Int64)
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)
import Hasql.Connection (Connection)
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
import Hasql.Session (QueryError)
import GameDTO
import Games


getGameHandler :: Pool -> Int64 -> Handler GameDTO
getGameHandler pool userId =  do
    res <- liftIO $ Games.findGame pool userId
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right as -> return $ Games.toGameDTO $ head as

getGamesHandler :: Pool -> Handler [GameDTO]
getGamesHandler pool = do
    res <- liftIO $ Games.findGames pool
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right as -> return $ map Games.toGameDTO as

createGameHandler :: Pool -> GameDTO -> Handler NoContent
createGameHandler p pl = do
        res <- liftIO $ Games.insertGame pl p
        case res of
            Left _ -> throwError err500
            Right [] -> throwError err403
            Right _ -> return NoContent

deleteGameHandler :: Pool -> Int64 -> Handler NoContent
deleteGameHandler _ userId =
    if userId == 0
        then throwError err404
        else pure NoContent

updateGameHandler :: Pool -> Int64 -> GameDTO -> Handler NoContent
updateGameHandler _ userId _ =
    if userId == 0
        then throwError err404
        else pure NoContent  