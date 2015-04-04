module GameManagement (
  Game(..),
  GameStatus,
  PendingGame(..),
  findGames,
  getStatus,
  allGames,
  ) where


import ClassyPrelude.Yesod


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
