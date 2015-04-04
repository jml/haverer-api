{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Game where

import Text.Blaze.Html (ToMarkup, toMarkup)
import Import

data GameStatus = Open | InProgress | Finished | Abandoned deriving (Eq, Show)

data BadStatus = BadStatus Text

type Seconds = Int

data PendingGame = OpenG { numPlayers :: Int, turnTimeout :: Seconds } deriving (Eq, Show)

data Game = Pending PendingGame deriving Show


instance ToJSON Game where
  toJSON _ = object []


instance ToJSON PendingGame where
  toJSON (OpenG { numPlayers = numPlayers,
                  turnTimeout = turnTimeout }) = object [ "numPlayers" .= numPlayers,
                                                          "turnTimeout" .= turnTimeout ]

instance FromJSON PendingGame where
  parseJSON (Object v) = OpenG <$> v .: "numPlayers" <*> v .: "turnTimeout"
  parseJSON _ = mzero

instance ToMarkup Game where
  toMarkup _ = "nothing to see here"

instance ToMarkup GameStatus where
  toMarkup = toMarkup . show


allGames :: [Game]
allGames = []


findGames :: [Game] -> Maybe GameStatus -> [Game]
findGames gs _ = gs


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


postGamesR :: Handler TypedContent
postGamesR = do
  newGame <- requireJsonBody :: Handler PendingGame
  $logInfo (pack $ show newGame)
  -- XXX: Should use sendResponseCreated, giving route to new game
  sendResponseStatus status201 ("CREATED" :: Text)
