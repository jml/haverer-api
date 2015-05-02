module Handler.GameSpec (spec) where

import Data.Aeson
import Network.HTTP.Types.Header (hAccept)
import Network.Wai.Test (simpleBody)

import Handler.Utilities (assertRedirect, doLogin, needsLogin, testUrl, StdMethod(POST))
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

  it "redirects when you try to POST without logging in" $ do
    needsLogin POST ("/games" :: Text)

  it "allows games to be created" $ do
    doLogin "testuser"
    postJson GamesR $ object [
        "numPlayers" .= (3 :: Int),
        "turnTimeout" .= (3600 :: Int)
        ]

    statusIs 201
    -- XXX: Hardcoding 0 unnecessarily and perhaps unhelpfully
    gameUrl <- testUrl "/game/0"
    assertHeader "Location" gameUrl

  it "404s on non-existent games" $ do
    get (GameR 0)
    statusIs 404

  it "has games" $ do
    let game = object [
          "numPlayers" .= (3 :: Int),
          "turnTimeout" .= (3600 :: Int)
          ]

    -- XXX: User ID of 1. Where does that come from?
    doLogin "testuser"
    postJson GamesR game
    getJson (GameR 0)

    assertJsonEqual $ object [
      "state" .= ("pending" :: Text),
      "numPlayers" .= (3 :: Int),
      "turnTimeout" .= (3600 :: Int),
      "creator" .= (1 :: Int),
      "players" .= [1 :: Int]
      ]

  it "people can sign-up to a game" $ do
    let game = object [
          "numPlayers" .= (3 :: Int),
          "turnTimeout" .= (3600 :: Int)
          ]

    -- XXX: User ID of 1. Where does that come from?
    doLogin "testuser"
    postJson GamesR game
    getJson (GameR 0)

    doLogin "anotheruser"
    postJson (GameR 0) (object [])

    assertRedirect "/game/0"

    getJson (GameR 0)

    assertJsonEqual $ object [
      "state" .= ("pending" :: Text),
      "numPlayers" .= (3 :: Int),
      "turnTimeout" .= (3600 :: Int),
      "creator" .= (1 :: Int),
      "players" .= [2 :: Int, 1 :: Int]
      ]

  it "signing up twice is same as signing up once" $ do
    let game = object [
          "numPlayers" .= (3 :: Int),
          "turnTimeout" .= (3600 :: Int)
          ]

    -- XXX: User ID of 1. Where does that come from?
    doLogin "testuser"
    postJson GamesR game
    getJson (GameR 0)

    doLogin "anotheruser"
    postJson (GameR 0) (object [])
    assertRedirect "/game/0"

    postJson (GameR 0) (object [])
    assertRedirect "/game/0"

    getJson (GameR 0)

    assertJsonEqual $ object [
      "state" .= ("pending" :: Text),
      "numPlayers" .= (3 :: Int),
      "turnTimeout" .= (3600 :: Int),
      "creator" .= (1 :: Int),
      "players" .= [2 :: Int, 1 :: Int]
      ]

  it "starts the game when everyone is signed up" $ do
    let game = object [
          "numPlayers" .= (2 :: Int),
          "turnTimeout" .= (3600 :: Int)
          ]

    -- XXX: User ID of 1. Where does that come from?
    doLogin "testuser"
    postJson GamesR game
    getJson (GameR 0)

    doLogin "anotheruser"
    postJson (GameR 0) (object [])

    assertRedirect "/game/0"

    getJson (GameR 0)

    assertJsonEqual $ object [
      "state" .= ("in-progress" :: Text),
      "turnTimeout" .= (3600 :: Int),
      "creator" .= (1 :: Int),
      "numPlayers" .= (2 :: Int),
      "players" .= [2, 1 :: Int]
      ]
