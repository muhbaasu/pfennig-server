{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module AuthServant where

import           Control.Monad.Trans.Either (EitherT)
import           Data.Maybe                 (fromMaybe)
import           Data.Text
import           Models                     ()
import           Servant

-- QueryParam "email" Text :>
type AuthAPI = "login" :> QueryParam "email" Text :> QueryParam "pass" Text :> Get '[JSON] Bool

type PublicAPI = AuthAPI

publicAPI :: Proxy PublicAPI
publicAPI = Proxy

login :: Maybe Text -> Maybe Text -> EitherT ServantErr IO Bool
login email pass =
  return $ fromMaybe False $ isValidLogin <$> email <*> pass

isValidLogin :: Text -> Text -> Bool
isValidLogin email pass = validEmail email && validPassword pass
  where validEmail = (== "special@some.com")
        validPassword = (== "passw0rd")

server :: Server PublicAPI
server = login
