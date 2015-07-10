{-# LANGUAGE DataKinds #-}
module Auth where

import qualified Data.ByteString.Lazy      as BS
import           Data.Monoid
import           Data.Text
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import           Data.Time.Calendar        (fromGregorian)
import           Data.Time.Clock           (UTCTime (..), addUTCTime,
                                            getCurrentTime, secondsToDiffTime)
import           Network.HTTP.Types.Status
import           Web.Cookie                (parseCookiesText)
import           Web.JWT
import           Web.Scotty                (ActionM, param, redirect, setHeader,
                                            status)

register :: ActionM ()
register = do
  email <- param "email"::ActionM Text
  pw    <- param "pass"::ActionM Text
  status created201
  return ()

login :: ActionM ()
login = do
  email <- param "email"::ActionM Text
  pw    <- param "pass"::ActionM Text
  if pw == "passw0rd"
    then authorize email
    else status unauthorized401
  return ()

key :: Secret
key = secret "all your base are belong to us"

unauthorize :: ActionM ()
unauthorize = do
  setHeader "Set-Cookie"
    "session=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT"

authorize :: Text -> ActionM ()
authorize username = do
  let cs = def { sub = stringOrURI username }
  let jwt = encodeSigned HS256 key cs
  setHeader "Set-Cookie" $ "session=" <>
    (TL.fromStrict jwt) <> "; path=/; HttpOnly"
  redirect "/main"

isCurrentlyValid :: JWT VerifiedJWT -> UTCTime -> Bool
isCurrentlyValid tkn now =
  let cl = claims tkn
      toUTC diff = addUTCTime (secondsSinceEpoch diff) $ UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
      notBefore = (now <) . toUTC <$> nbf cl
      notExpired = (now >) . toUTC <$> Web.JWT.exp cl
      valid = (&&) <$> notBefore <*> notExpired
  in Just True == valid

isAuthorized :: TL.Text -> Bool
isAuthorized cookie =
  let bs = BS.toStrict $ TL.encodeUtf8 cookie
      cs = parseCookiesText bs
      token = lookup "session" cs
  in case token of
      Nothing -> False
      Just val ->
        let mJwt = decodeAndVerifySignature key val
        in maybe False (\jwt -> True) mJwt
