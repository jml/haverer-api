module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


type Seconds = Int

data PendingGame = OpenG { numPlayers :: Int
                         , turnTimeout :: Seconds
                         } deriving (Eq, Show)

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
