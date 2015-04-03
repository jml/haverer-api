module Handler.Game where

import Import

data Game = Game

instance ToJSON Game where
  toJSON Game { .. } = object []


getGamesR :: Handler TypedContent
getGamesR = selectRep $ do
  provideRep $ defaultLayout $ $(widgetFile "games")
  provideJson $ ([] :: [Game])
