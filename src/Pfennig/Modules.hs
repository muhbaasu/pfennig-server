module Modules where

import qualified Data.Text as T
import           Models

data RegisterError = RegisterError

data AuthModule = Auth {
    registerUser :: UserFields -> IO (Either T.Text User)
  , loginUser    :: UserFields -> IO (Either T.Text User) }
