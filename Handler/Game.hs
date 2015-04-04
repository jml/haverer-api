{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Game where

import GameManagement
import Text.Blaze.Html (ToMarkup, toMarkup)
import Import


instance ToMarkup Game where
  toMarkup _ = "nothing to see here"


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
