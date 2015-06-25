{-# LANGUAGE DataKinds #-}
module Handlers where

import           App
import           Control.Error.Safe        (justErr)
import           Control.Monad             (join)
import           Data.Aeson                (object, (.=))
import           Data.Bifunctor            (bimap)
import qualified Hasql                     as H
import           Models
import           Network.HTTP.Types.Status
import           Queries
import           Web.Scotty                (ActionM, json, jsonData, param,
                                            status)


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
    dbResult <- s $ H.tx Nothing (insertExpenditure fields)
    case dbResult of
      (Left err) -> do
        json $ object [ "error" .= show err ]
        status badRequest400
      (Right expenditure) -> json expenditure

getExpenditures :: RouteHandler
getExpenditures (AppConfig s) = do
  dbResult <- s $ H.tx Nothing allExpenditures
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
  count <- s $ H.tx Nothing $ delExpenditure $ ExpenditureId eid -- count should probably be checked
  case count of
    (Right 1) -> status accepted202
    (Right 0) -> status notFound404
    (Left err) -> do
      json $ object [ "error" .= show err ]
      status badRequest400
    _ -> status internalServerError500 -- something weird happened
