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
                    , players :: [UserId]
                    }
          | InProgress { turnTimeout :: Seconds
                       , creator :: UserId
                       }
          deriving Show


createGame :: UserId -> GameCreationRequest -> Game
createGame creator req = Pending { numPlayers = reqNumPlayers req
                                 , turnTimeout = reqTurnTimeout req
                                 , creator = creator
                                 , players = [creator]
                                 }


beginGame :: Game -> Game
beginGame (Pending { turnTimeout = turnTimeout, creator = creator}) =
  InProgress { turnTimeout = turnTimeout
             , creator = creator
             }
beginGame (InProgress { .. }) = error "Cannot begin game that's already going"

-- XXX: Use lenses for this?
-- XXX: Non-pending game once they've joined.
-- XXX: Tests
joinGame :: UserId -> Game -> Game
joinGame newPlayer p@(Pending { players = players, numPlayers = numPlayers }) =
  let newPlayers = newPlayer:players
      currentPlayers = length newPlayers in
   case compare currentPlayers numPlayers of
    LT -> p { players = newPlayers }
    EQ -> beginGame p
    GT -> error "Game is already full"
joinGame _ _ = error "Cannot join game that's already started"


instance ToJSON Game where
  toJSON (Pending numPlayers turnTimeout creator players) = object [
    "state" .= ("pending" :: Text),
    "numPlayers" .= numPlayers,
    "turnTimeout" .= turnTimeout,
    "creator" .= creator,
    "players" .= players
    ]
  toJSON (InProgress turnTimeout creator) = object [
    "state" .= ("in-progress" :: Text),
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
