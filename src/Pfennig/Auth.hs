{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Auth where


import           App
import qualified Authentication as A
import           Models

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Either (left)
import           Control.Monad.Reader       (ask)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import           Data.Time.Calendar         (fromGregorian)
import           Data.Time.Clock            (NominalDiffTime, UTCTime (..),
                                             addUTCTime, diffUTCTime,
                                             getCurrentTime, secondsToDiffTime,
                                             secondsToDiffTime)
import           Servant
import           Web.Cookie                 (parseCookiesText)
import qualified Web.JWT

type AuthAPI = "grant" :> ReqBody '[JSON] UserFields :> Post '[JSON] Web.JWT.JSON
          :<|> "register" :> ReqBody '[JSON] UserFields :> Post '[JSON] ()

authAPI :: Proxy AuthAPI
authAPI = Proxy

server :: ServerT AuthAPI RouteM
server = grant :<|> register

key :: Web.JWT.Secret
key = Web.JWT.secret "all your base are belong to us"

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

sessionDuration :: NominalDiffTime
sessionDuration = fromInteger $ 60 * 60 * 60

register :: UserFields -> RouteM ()
register userfields = do
  (AppConfig session) <- ask
  r <- liftIO $ A.registerUser session userfields
  case r of
    Right _-> return ()
    Left _ -> lift $ left err500

grant :: UserFields -> RouteM Web.JWT.JSON
grant u = do
  (AppConfig session) <- ask
  user <- liftIO $ A.loginUser session u
  now <- liftIO getCurrentTime
  case user of
    Right _ -> return $ createToken (_usrFEmail u) now
    Left _ -> lift $ left err401

createToken :: UserEmail -> UTCTime -> Web.JWT.JSON
createToken (UserEmail address) now =
  let jwtNbf = Web.JWT.intDate 0
      jwtExp = Web.JWT.intDate $ diffUTCTime now epoch + sessionDuration
      cs = Web.JWT.def {
           Web.JWT.sub = Web.JWT.stringOrURI address
         , Web.JWT.nbf = jwtNbf
         , Web.JWT.exp = jwtExp }
  in Web.JWT.encodeSigned Web.JWT.HS256 key cs

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
  let bs = BL.toStrict $ TL.encodeUtf8 cookie
      cs = parseCookiesText bs
      token = lookup "session" cs
  in case token of
      Nothing -> False
      Just val ->
        let mJwt = Web.JWT.decodeAndVerifySignature key val
        in maybe False (`isCurrentlyValid` now) mJwt
