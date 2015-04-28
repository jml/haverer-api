{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Game where

import Data.Aeson (encode)
import Text.Blaze.Html (ToMarkup, toMarkup)
import qualified Text.Blaze.Html5              as H
import Import

import Data.Vector ((!?), (//))
import qualified Data.Vector as Vector


instance ToMarkup Game where
  toMarkup = H.code . toHtml . decodeUtf8 . encode . toJSON


getGame :: Int -> Handler (Maybe Game)
getGame n = do
  allGames <- appAllGames <$> getYesod
  matchingGames <- atomically $ readTVar allGames
  return $ matchingGames !? n


modifyGame :: Int -> (Game -> Game) -> Handler ()
modifyGame n f = do
  allGames <- appAllGames <$> getYesod
  atomically $ modifyTVar' allGames $
    \games -> case games !? n of
               Nothing -> games
               Just g -> games // [(n, f g)]


getGameR :: Int -> Handler TypedContent
getGameR n = do
  matchingGame <- getGame n
  case matchingGame of
   Just game -> defaultLayoutJson $(widgetFile "game") (returnJson game)
   Nothing -> notFound


postGameR :: Int -> Handler TypedContent
postGameR n = do
  user <- requireAuthId
  $(logInfo) (pack $ show user)
  modifyGame n (joinGame user)
  redirect (GameR n)


getGamesR :: Handler TypedContent
getGamesR = do
  allGames <- appAllGames <$> getYesod
  matchingGames <- atomically $ readTVar allGames
  defaultLayoutJson $(widgetFile "games") (returnJson matchingGames)


postGamesR :: Handler TypedContent
postGamesR = do
  newGame <- createGame <$> requireAuthId <*> requireJsonBody
  allGames <- appAllGames <$> getYesod
  newId <- atomically $ do
    currentGames <- readTVar allGames
    writeTVar allGames (Vector.snoc currentGames newGame)
    return $ Vector.length currentGames
  sendResponseCreated (GameR newId)
