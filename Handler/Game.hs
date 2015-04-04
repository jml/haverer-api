{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Game where

import Import

data Game = Game

instance ToJSON Game where
  toJSON Game { .. } = object []


allGames :: [Game]
allGames = []


findGames :: [Game] -> Maybe GameStatus -> [Game]
findGames _ _ = []


data GameStatus = Open | InProgress | Finished | Abandoned deriving (Eq, Show)


data BadStatus = BadStatus Text


textToStatus :: Text -> Either BadStatus GameStatus
textToStatus "open" = return Open
textToStatus x = Left $ BadStatus x


getStatus :: Maybe Text -> Either BadStatus (Maybe GameStatus)
getStatus Nothing = Right Nothing
getStatus (Just x) =
  case textToStatus x of
   Right s -> Right (Just s)
   Left e -> Left e


getGamesR :: Handler TypedContent
getGamesR = do
  status' <- getStatus <$> lookupGetParam "status"
  status <- case status' of
    Left e -> invalidArgs ["status"]
    Right s -> return s
  let matchingGames = findGames allGames status
  selectRep $ do
    provideRep $ defaultLayout $ $(widgetFile "games")
    provideJson $ matchingGames
