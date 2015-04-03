module Handler.Game where

import Import

getGamesR :: Handler Html
getGamesR = do
  defaultLayout $ $(widgetFile "games")
