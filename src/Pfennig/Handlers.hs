{-# LANGUAGE DataKinds #-}
module Handlers where

import           App
import           Layout                    (readCSS)
import           Models
import           Queries                   as Q

import           Control.Error.Safe        (justErr)
import           Control.Monad             (join)
import           Data.Aeson                (object, (.=))
import           Data.Bifunctor            (bimap)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Text
import qualified Hasql                     as H
import           Network.HTTP.Types.Status
import           Web.Scotty                (ActionM, json, jsonData, param, raw,
                                            setHeader, status)

getCss :: ByteString -> ActionM ()
getCss css = do
  raw css
  setHeader "Content-Type" "text/css"
  setHeader "Cache-Control" "no-transform,public,max-age=300,s-maxage=900"
  status ok200
  return ()

getExpenditure :: RouteHandler
getExpenditure (AppConfig s) = do
  eid <- param "id"
  expenditure <- fmap mapErrText $ s $ H.tx Nothing $ singleExpenditure $ ExpenditureId eid
  case expenditure of
    (Left err) -> do
      json $ object [ "error" .= show err ]
      status notFound404
    (Right ex) ->json ex
  where mapErrText = join . bimap unpackSessionError (justErr "Not found")

createExpenditure :: RouteHandler
createExpenditure (AppConfig s) = do
    fields <- jsonData :: ActionM (ExpenditureFields 'Database)
    dbResult <- s $ H.tx Nothing (Q.insertExpenditure fields)
    case dbResult of
      (Left err) -> do
        json $ object [ "error" .= show err ]
        status badRequest400
      (Right expenditure) -> json expenditure

getExpenditures :: RouteHandler
getExpenditures (AppConfig s) = do
  dbResult <- s $ H.tx Nothing Q.allExpenditures
  case dbResult of
    (Left err) -> do
      json $ object [ "error" .= show err ]
      status badRequest400
    (Right expenditure) -> json expenditure

updateExpenditure :: RouteHandler
updateExpenditure =
  return $ do
    _ <- param "id" :: ActionM Integer
    expenditure <- jsonData :: ActionM (ExpenditureFields 'Database)
    json expenditure
    status accepted202

deleteExpenditure :: RouteHandler
deleteExpenditure (AppConfig s) = do
  eid <- param "id" :: ActionM Int
  count <- s $ H.tx Nothing $ Q.deleteExpenditure $ ExpenditureId eid -- count should probably be checked
  case count of
    (Right 1) -> status accepted202
    (Right 0) -> status notFound404
    (Left err) -> do
      json $ object [ "error" .= show err ]
      status badRequest400
    _ -> status internalServerError500 -- something weird happened
