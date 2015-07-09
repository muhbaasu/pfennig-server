{-# LANGUAGE DataKinds #-}
module Auth where

import qualified Data.ByteString.Lazy      as BS
import           Data.Monoid
import           Data.Text
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
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
