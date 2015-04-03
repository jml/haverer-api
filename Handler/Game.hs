{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Game where

import Import

data Game = Game

instance ToJSON Game where
  toJSON Game { .. } = object []


findGames :: [Game]
findGames = []


data GameStatus = Open | InProgress | Finished | Abandoned deriving (Eq, Show)

textToStatus :: Text -> Maybe GameStatus
textToStatus "open" = return Open
textToStatus _ = Nothing


getGamesR :: Handler TypedContent
getGamesR = do
  status' <- lookupGetParam "status"
  s <- case status' of
   Nothing -> do
     $logInfo "all"
     return Open
   Just status'' ->
     case textToStatus status'' of
      Nothing -> invalidArgs ["status"]
      Just status -> do
        $logInfo (pack (show status))
        return status
  selectRep $ do
    provideRep $ defaultLayout $ $(widgetFile "games")
    provideJson $ findGames
