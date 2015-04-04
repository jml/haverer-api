{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Game where

import Data.Aeson (encode)
import GameManagement
import Text.Blaze.Html (ToMarkup, toMarkup)
import qualified Text.Blaze.Html5              as H
import Import


instance ToMarkup Game where
  toMarkup = H.code . toHtml . decodeUtf8 . encode . toJSON


getGamesR :: Handler TypedContent
getGamesR = do
  allGames <- appAllGames <$> getYesod
  matchingGames <- atomically $ readTVar allGames
  selectRep $ do
    provideRep $ defaultLayout $ $(widgetFile "games")
    provideJson $ matchingGames


postGamesR :: Handler TypedContent
postGamesR = do
  newGame <- requireJsonBody :: Handler PendingGame
  $logInfo (pack $ show newGame)
  allGames <- appAllGames <$> getYesod
  atomically $ do
    currentGames <- readTVar allGames
    writeTVar allGames (Pending newGame:currentGames)
  -- XXX: Should use sendResponseCreated, giving route to new game
  sendResponseStatus status201 ("CREATED" :: Text)
