module GameManagement (
  Game(..),
  PendingGame(..)
  ) where


import ClassyPrelude.Yesod


type Seconds = Int

data PendingGame = OpenG { numPlayers :: Int, turnTimeout :: Seconds } deriving (Eq, Show)

data Game = Pending PendingGame deriving Show


instance ToJSON Game where
  toJSON (Pending g) = object [
    "state" .= ("pending" :: Text),
    "numPlayers" .= numPlayers g,
    "turnTimeout" .= turnTimeout g
    ]


instance ToJSON PendingGame where
  toJSON (OpenG { numPlayers = numPlayers,
                  turnTimeout = turnTimeout }) = object [ "numPlayers" .= numPlayers,
                                                          "turnTimeout" .= turnTimeout ]

instance FromJSON PendingGame where
  parseJSON (Object v) = OpenG <$> v .: "numPlayers" <*> v .: "turnTimeout"
  parseJSON _ = mzero
