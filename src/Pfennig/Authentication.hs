{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types  #-}

module Authentication where

import           App
import           Control.Error.Safe     (justErr)
import           Control.Monad          (join)
import           Control.Monad.IO.Class (liftIO)
import           Crypto.PasswordStore
import           Data.Bifunctor         (bimap)
import qualified Data.ByteString        as BS
import           Data.Text
import qualified Data.Text              as T
import           Data.Time.LocalTime    (LocalTime)
import qualified Hasql                  as H
import qualified Hasql.Postgres         as HP
import           Models
import qualified Modules                as M
import           Queries

makeModule :: AppConfig -> M.AuthModule
makeModule (AppConfig session) = M.Auth {
    M.registerUser = registerUser session
  , M.loginUser = loginUser session }

cryptoStrength :: Int
cryptoStrength = 17

registerUser :: PostgresSession -> UserFields -> IO (Either T.Text User)
registerUser session userFields = do
  hashedPw <- liftIO $ makePassword password cryptoStrength
  newUserRow <- session $ H.tx Nothing $ insertNewUser email hashedPw
  return $ extractUser newUserRow
  where extractUser = join . bimap unpackSessionError (justErr "not found")
        password = _usrFPassword userFields
        email = _usrFEmail userFields

insertNewUser :: UserEmail -> BS.ByteString -> H.Tx HP.Postgres s (Maybe User)
insertNewUser (UserEmail email) password = do
  newUser <- H.maybeEx $
             [H.stmt|insert into users (email, password)
                     values (?, ?)
                     returning * |] email password
  return $ rowToUser <$> newUser

rowToUser :: (Int, LocalTime, LocalTime, Text, BS.ByteString) -> User
rowToUser (i, created, updated, email, pw) = User (UserId i) created updated (UserEmail email) pw

loginUser :: PostgresSession -> UserFields -> IO (Either T.Text User)
loginUser session (UserFields email pw) = do
  user <- session $ H.tx Nothing $ selectUser email
  return $ join $ verify <$> extractUser user
  where extractUser = join . bimap unpackSessionError (justErr "User not found")
        verify user = if verifyPassword pw (_usrPassword user)
                        then Right user
                        else Left "Invalid credentials"

selectUser :: UserEmail -> H.Tx HP.Postgres s (Maybe User)
selectUser (UserEmail email) = do
  user <- H.maybeEx $
            [H.stmt|select * from users
                    where email = ?|] email
  return $ rowToUser <$> user
