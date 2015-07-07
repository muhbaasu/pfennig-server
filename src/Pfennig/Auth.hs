{-# LANGUAGE DataKinds #-}
module Auth where

import           App
import           Data.Text
import           Network.HTTP.Types.Status
import           Web.Scotty                (ActionM, param, status)

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
    then status ok200
    else status unauthorized401
  return ()
