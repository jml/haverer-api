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

import Data.Vector ((!?))
import qualified Data.Vector as Vector


instance ToMarkup Game where
  toMarkup = H.code . toHtml . decodeUtf8 . encode . toJSON


getGameR :: Int -> Handler TypedContent
getGameR n = do
  allGames <- appAllGames <$> getYesod
  matchingGames <- atomically $ readTVar allGames
  case matchingGames !? n of
   Just game -> defaultLayoutJson $(widgetFile "game") (returnJson game)
   Nothing -> notFound


getGamesR :: Handler TypedContent
getGamesR = do
  allGames <- appAllGames <$> getYesod
  matchingGames <- atomically $ readTVar allGames
  defaultLayoutJson $(widgetFile "games") (returnJson matchingGames)


postGamesR :: Handler TypedContent
postGamesR = do
  newGame <- createGame <$> requireJsonBody
  allGames <- appAllGames <$> getYesod
  newId <- atomically $ do
    currentGames <- readTVar allGames
    writeTVar allGames (Vector.snoc currentGames newGame)
    return $ Vector.length currentGames
  sendResponseCreated (GameR newId)
