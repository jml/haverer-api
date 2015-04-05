module Handler.GameSpec (spec) where

import Data.Aeson
import Network.HTTP.Types.Header (hAccept)
import Network.Wai.Test (simpleBody)

import Settings (appRoot)
import TestImport


getJson urlThing = request $ do
  addRequestHeader (hAccept, "application/json")
  setMethod "GET"
  setUrl urlThing


postJson urlThing value = request $ do
  addRequestHeader (hAccept, "application/json")
  setUrl urlThing
  setMethod "POST"
  setRequestBody $ encode value


assertJsonEqual :: (FromJSON a, Eq a, Show a) => a -> YesodExample site ()
assertJsonEqual expected = withResponse assertThing
  where assertThing response =
          let observed = decode $ simpleBody response
              message = show expected ++ " != " ++ show observed
          in assertEqual message (Just expected) observed


spec :: Spec
spec = withApp $ do
  it "starts off with no games" $ do
    getJson GamesR

    statusIs 200
    bodyEquals "[]"

  it "allows games to be created" $ do
    postJson GamesR $ object [
        "numPlayers" .= (3 :: Int),
        "turnTimeout" .= (3600 :: Int)
        ]

    statusIs 201
    testAppRoot <- encodeUtf8 <$> appRoot <$> appSettings <$> getTestYesod
    -- XXX: Hardcoding 0 unnecessarily and perhaps unhelpfully
    assertHeader "Location" (testAppRoot ++ "/game/0")

  it "404s on non-existent games" $ do
    get (GameR 0)
    statusIs 404

  it "has games" $ do
    let game = object [
          "numPlayers" .= (3 :: Int),
          "turnTimeout" .= (3600 :: Int)
          ]
    postJson GamesR game

    getJson (GameR 0)

    -- XXX: Figure out how to update a JSON object with a new field.
    assertJsonEqual $ object [
      "state" .= ("pending" :: Text),
      "numPlayers" .= (3 :: Int),
      "turnTimeout" .= (3600 :: Int)
      ]