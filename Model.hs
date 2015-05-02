module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import qualified Haverer.Game as H
import Haverer.Player (toPlayers, toPlayerSet)

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


data Game = Pending { turnTimeout :: Seconds
                    , creator :: UserId
                    , _numPlayers :: Int
                    , _players :: [UserId]
                    }
          | InProgress { turnTimeout :: Seconds
                       , creator :: UserId
                       , game :: H.Game UserId
                       }
          deriving Show


createGame :: UserId -> GameCreationRequest -> Game
createGame creator req = Pending { turnTimeout = reqTurnTimeout req
                                 , creator = creator
                                 , _numPlayers = reqNumPlayers req
                                 , _players = [creator]
                                 }

players :: Game -> [UserId]
players (Pending { _players = players' }) = players'
players (InProgress { .. }) = toPlayers $ H.players $ game

numPlayers :: Game -> Int
numPlayers (Pending { _numPlayers = numPlayers' }) = numPlayers'
numPlayers g@(InProgress { .. }) = length $ players g


beginGame :: Game -> Game
beginGame (Pending { .. }) =
  InProgress { turnTimeout = turnTimeout
             , creator = creator
             , game = H.makeGame playerSet
             }
  where playerSet = case toPlayerSet _players of
                     Left e -> error (show e)
                     Right r -> r
beginGame (InProgress { .. }) = error "Cannot begin game that's already going"

-- XXX: Use lenses for this?
-- XXX: Direct tests
joinGame :: UserId -> Game -> Game
joinGame newPlayer p@(Pending { .. }) =
  let players' = players p
      newPlayers = if (newPlayer `oelem` players') then players' else newPlayer:players'
      currentPlayers = length newPlayers in
   case compare currentPlayers (numPlayers p) of
    LT -> p { _players = newPlayers }
    EQ -> beginGame $ p { _players = newPlayers }
    GT -> error "Game is already full"
joinGame _ _ = error "Cannot join game that's already started"


instance ToJSON Game where
  toJSON (Pending { .. }) = object [
    "state" .= ("pending" :: Text),
    "turnTimeout" .= turnTimeout,
    "creator" .= creator,
    "numPlayers" .= _numPlayers,
    "players" .= _players
    ]
  toJSON g@(InProgress { .. }) = object [
    "state" .= ("in-progress" :: Text),
    "turnTimeout" .= turnTimeout,
    "creator" .= creator,
    "numPlayers" .= (numPlayers g),
    "players" .= (players g)
    ]


instance ToJSON GameCreationRequest where
  toJSON (GameCreationRequest { .. }) = object [ "numPlayers" .= reqNumPlayers
                                               , "turnTimeout" .= reqTurnTimeout
                                               ]


instance FromJSON GameCreationRequest where
  parseJSON (Object v) = GameCreationRequest <$> v .: "numPlayers" <*> v .: "turnTimeout"
  parseJSON _ = mzero
