{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers where

import           App
import           Data.Aeson                ((.=), object)
import           Data.Time.Format          (defaultTimeLocale, parseTimeOrError)
import           Data.Time.LocalTime       (LocalTime)
import qualified Hasql                     as H
import qualified Hasql.Postgres            as HP
import           Models
import           Network.HTTP.Types.Status
import           Web.Scotty                (ActionM, json, jsonData, param,
                                            status)

insertExpenditure :: ExpenditureFields -> H.Tx HP.Postgres s Expenditure
insertExpenditure (ExpenditureFields description) = do
  (id', created, updated, desc) <-
    H.singleEx $ [H.stmt|insert into expenditures (description)
                         values (?) returning *|] description
  let eid = ExpenditureId id'
  let fields = ExpenditureFields desc
  return $ Expenditure eid created updated fields

randomLocalTime :: LocalTime
randomLocalTime = parseTimeOrError False defaultTimeLocale "%F" "2005-04-07"

getExpenditures :: RouteHandler
getExpenditures =
  return $ json [Expenditure (ExpenditureId 5) randomLocalTime randomLocalTime fields]
  where fields = ExpenditureFields "Hello!"

getExpenditure :: RouteHandler
getExpenditure =
  return $ do
    eid <- param "id"
    json $ Expenditure (ExpenditureId eid) randomLocalTime randomLocalTime fields
  where fields = ExpenditureFields "Hello!"

createExpenditure :: RouteHandler
createExpenditure (AppConfig s) = do
    fields <- jsonData :: ActionM ExpenditureFields
    dbResult <- s $ H.tx Nothing (insertExpenditure fields)
    case dbResult of
      (Left err) -> do
        json $ object [ "error" .= show err ]
        status badRequest400
      (Right expenditure) -> json expenditure

updateExpenditure :: RouteHandler
updateExpenditure =
  return $ do
    _ <- param "id" :: ActionM Integer
    expenditure <- jsonData :: ActionM ExpenditureFields
    json expenditure
    status accepted202

deleteExpenditure :: RouteHandler
deleteExpenditure =
  return $ do
    _ <- param "id" :: ActionM Integer
    status accepted202
