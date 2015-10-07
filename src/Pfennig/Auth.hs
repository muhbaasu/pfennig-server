{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Auth where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either (EitherT, left)
import qualified Data.ByteString.Lazy       as BS
import           Data.Text
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import           Data.Time.Calendar         (fromGregorian)
import           Data.Time.Clock            (NominalDiffTime, UTCTime (..),
                                             addUTCTime, diffUTCTime,
                                             getCurrentTime, secondsToDiffTime,
                                             secondsToDiffTime)
import           Models                     ()
import           Servant
import           Web.Cookie                 (parseCookiesText)
import qualified Web.JWT

type AuthAPI = "auth" :> QueryParam "email" Text :> QueryParam "pass" Text :> Post '[JSON] Web.JWT.JSON
          :<|> "register" :> QueryParam "email" Text :> QueryParam "pass" Text :> Post '[JSON] Bool

server :: Server AuthAPI
server = auth
    :<|> register

key :: Web.JWT.Secret
key = Web.JWT.secret "all your base are belong to us"

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

sessionDuration :: NominalDiffTime
sessionDuration = fromInteger $ 60 * 60 * 60

-- | retrieve authorization token
auth :: Maybe Text -> Maybe Text -> EitherT ServantErr IO Web.JWT.JSON
auth email pass =  do
  now <- liftIO getCurrentTime
  case authorize now <$> email of
    Nothing -> left err401
    Just x -> return x

register :: Maybe Text -> Maybe Text -> EitherT ServantErr IO Bool
register email pass = return True

isValidLogin :: Text -> Text -> Bool
isValidLogin email pass = validEmail email && validPassword pass
  where validEmail = (== "special@some.com")
        validPassword = (== "passw0rd")

isCurrentlyValid :: Web.JWT.JWT Web.JWT.VerifiedJWT -> UTCTime -> Bool
isCurrentlyValid tkn now =
  let cl = Web.JWT.claims tkn
      toUTC diff = addUTCTime (Web.JWT.secondsSinceEpoch diff) epoch
      notBefore  = (now >=) . toUTC <$> Web.JWT.nbf cl
      notExpired = (now <=) . toUTC <$> Web.JWT.exp cl
      valid = (&&) <$> notBefore <*> notExpired
  in Just True == valid

isAuthorized :: UTCTime -> TL.Text -> Bool
isAuthorized now cookie =
  let bs = BS.toStrict $ TL.encodeUtf8 cookie
      cs = parseCookiesText bs
      token = lookup "session" cs
  in case token of
      Nothing -> False
      Just val ->
        let mJwt = Web.JWT.decodeAndVerifySignature key val
        in maybe False (`isCurrentlyValid` now) mJwt

authorize :: UTCTime -> Text -> Web.JWT.JSON
authorize now username =
  let jwtNbf = Web.JWT.intDate 0
      jwtExp = Web.JWT.intDate $ diffUTCTime now epoch + sessionDuration
      cs = Web.JWT.def {
              Web.JWT.sub = Web.JWT.stringOrURI username
            , Web.JWT.nbf = jwtNbf
            , Web.JWT.exp = jwtExp }
  in Web.JWT.encodeSigned Web.JWT.HS256 key cs
