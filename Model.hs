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

data GameCreationRequest = GameCreationRequest { reqNumPlayers :: Int
                                               , reqTurnTimeout :: Seconds
                                               } deriving (Eq, Show)


data Game = Pending { numPlayers :: Int
                    , turnTimeout :: Seconds
                    , creator :: UserId
                    } deriving Show


createGame :: UserId -> GameCreationRequest -> Game
createGame creator req = Pending { numPlayers = reqNumPlayers req
                                 , turnTimeout = reqTurnTimeout req
                                 , creator = creator
                                 }


instance ToJSON Game where
  toJSON (Pending numPlayers turnTimeout creator) = object [
    "state" .= ("pending" :: Text),
    "numPlayers" .= numPlayers,
    "turnTimeout" .= turnTimeout,
    "creator" .= creator
    ]


instance ToJSON GameCreationRequest where
  toJSON (GameCreationRequest {
             reqNumPlayers = numPlayers,
             reqTurnTimeout = turnTimeout
             }) = object [ "numPlayers" .= numPlayers,
                           "turnTimeout" .= turnTimeout ]


instance FromJSON GameCreationRequest where
  parseJSON (Object v) = GameCreationRequest <$> v .: "numPlayers" <*> v .: "turnTimeout"
  parseJSON _ = mzero
