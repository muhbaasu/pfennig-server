{-# LANGUAGE DataKinds #-}
module Auth where

import           Data.Text
import           Network.HTTP.Types.Status
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
    then authorized
    else status unauthorized401
  return ()

authorized :: ActionM ()
authorized = do
  setHeader "Set-Cookie" "authorized"
  redirect "/main"
