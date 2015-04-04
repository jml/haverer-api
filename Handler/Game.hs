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

instance ToMarkup GameStatus where
  toMarkup = toMarkup . show


getGamesR :: Handler TypedContent
getGamesR = do
  status' <- getStatus <$> lookupGetParam "status"
  status <- case status' of
    Left e -> invalidArgs ["status"]
    Right s -> return s
  allGames <- appAllGames <$> getYesod
  allGames' <- atomically $ readTVar allGames
  let matchingGames = findGames allGames' status
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
