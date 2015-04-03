{-# LANGUAGE ScopedTypeVariables #-}

module Handlers where

import           App
import           Data.DateTime             (fromSeconds)
import           Models
import           Network.HTTP.Types.Status
import           Web.Scotty                (ActionM, json, jsonData, param,
                                            status)

getExpenditures :: RouteHandler
getExpenditures =
  return $ json [Expenditure (ExpenditureId 5) (fromSeconds 0) (fromSeconds 100) fields]
  where fields = ExpenditureFields "Hello!"

getExpenditure :: RouteHandler
getExpenditure =
  return $ do
    eid <- param "id"
    json $ Expenditure (ExpenditureId eid) (fromSeconds 0) (fromSeconds 100) fields
  where fields = ExpenditureFields "Hello!"

createExpenditure :: RouteHandler
createExpenditure =
  return $ do
    expenditure <- jsonData :: ActionM ExpenditureFields
    json expenditure
    status accepted202

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
