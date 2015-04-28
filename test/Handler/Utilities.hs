{-# LANGUAGE PackageImports #-}

module Handler.Utilities (
    assertFailure,
    assertRedirect,
    urlPath,
    needsLogin,
    doLogin,
    StdMethod(..),
    testUrl
) where

import Settings (appRoot)
import TestImport

import Yesod.Core         (RedirectUrl)
import "network-uri" Network.URI        (URI(uriPath), parseURI)
import Network.HTTP.Types (StdMethod(..), renderStdMethod, Status(..))
import Network.Wai.Test   (SResponse(..))


-- | Get the approot of the test yesod
testRoot :: YesodExample App Text
testRoot = appRoot <$> appSettings <$> getTestYesod

-- | Get the URL relative to the test yesod
testUrl :: Text -> YesodExample App ByteString
testUrl path = do
  rootUrl <- testRoot
  return $ encodeUtf8 $ (rootUrl ++ path)


-- Force failure by swearing that black is white, and pigs can fly...
assertFailure :: String -> YesodExample App ()
assertFailure msg = assertEqual msg True False


-- | Assert that we get redirected to a URL.
assertRedirect url = do
  fullUrl <- testUrl url
  maybeUrl <- extractLocation
  assertEqual
    ("Should be redirected to " ++ (show fullUrl) ++ ", got " ++ (show maybeUrl))
    (Just fullUrl) maybeUrl

-- Convert an absolute URL (eg extracted from responses) to just the path
-- for use in test requests.
urlPath :: Text -> Text
urlPath = pack . maybe "" uriPath . parseURI . unpack


-- Internal use only - actual urls are ascii, so exact encoding is irrelevant
urlPathB :: ByteString -> Text
urlPathB = urlPath . decodeUtf8


-- Stages in login process, used below
firstRedirect :: RedirectUrl App url =>
                 StdMethod -> url -> YesodExample App (Maybe ByteString)
firstRedirect method url = do
  request $ do
    setMethod $ renderStdMethod method
    setUrl url
  extractLocation  -- We should get redirected to the login page


assertLoginPage :: ByteString -> YesodExample App ()
assertLoginPage loc = do
  loginUrl <- testUrl "/auth/login"
  assertEqual "correct login redirection location" loginUrl loc
  get $ urlPathB loc
  statusIs 200
  bodyContains "Login"


submitLogin :: Text -> YesodExample App (Maybe ByteString)
submitLogin user = do
  -- Ideally we would extract this url from the login form on the current page
  submitUrl <- testUrl "/auth/page/dummy"
  request $ do
    setMethod "POST"
    setUrl $ urlPathB submitUrl
    addPostParam "ident" user
  extractLocation  -- Successful login should redirect to the home page


extractLocation :: YesodExample App (Maybe ByteString)
extractLocation = do
    withResponse ( \ SResponse { simpleStatus = s, simpleHeaders = h } -> do
                        let code = statusCode s
                        assertEqual ("Expected a 302 or 303 redirection status "
                                     ++ "but received " ++ show code)
                                    (code `oelem` [302,303])
                                    True
                        return $ lookup "Location" h
                 )

-- Check that accessing the url with the given method requires login, and
-- that it redirects us to what looks like the login page.  Note that this is
-- *not* an ajax request, whatever the method, so the redirection *should*
-- result in the HTML login page.
--
needsLogin :: RedirectUrl App url => StdMethod -> url -> YesodExample App ()
needsLogin method url = do
    mbloc <- firstRedirect method url
    maybe (assertFailure "Should have location header") assertLoginPage mbloc

-- Do a login (using hashdb auth).  This just attempts to go to the home
-- url, and follows through the login process.  It should probably be the
-- first thing in each "it" spec.
--
-- | Actually log in.
-- |
-- | This works by hardcoding the dummy login sequence.
doLogin :: Text -> YesodExample App ()
doLogin user = do
  mbloc2 <- submitLogin user
  case mbloc2 of
   Nothing -> assertFailure "Should have second location header"
   Just _ -> return ()
