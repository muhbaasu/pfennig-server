{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers where

import           App
import           Control.Error.Safe        (justErr)
import           Data.Aeson                (object, (.=))
import           Data.Bifunctor            (bimap)
import qualified Data.Text                 as T
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
  row <-
    H.singleEx $ [H.stmt|insert into expenditures (description)
                         values (?) returning *|] description
  return $ rowToExpenditure row

allExpenditures :: H.Tx HP.Postgres s [Expenditure]
allExpenditures = do
  rows <-
    H.listEx $ [H.stmt|select * from expenditures|]
  return $ fmap rowToExpenditure rows

singleExpenditure :: ExpenditureId -> H.Tx HP.Postgres s (Maybe Expenditure)
singleExpenditure (ExpenditureId eid) = do
  row <-
     H.maybeEx $ [H.stmt|select * from expenditures where eid = ?|] eid
  return $ fmap rowToExpenditure row

rowToExpenditure :: (Int, LocalTime, LocalTime, T.Text) -> Expenditure
rowToExpenditure (id', created, updated, desc) =
  let eid = ExpenditureId id'
      fields = ExpenditureFields desc
  in Expenditure eid created updated fields

randomLocalTime :: LocalTime
randomLocalTime = parseTimeOrError False defaultTimeLocale "%F" "2005-04-07"

getExpenditures :: RouteHandler
getExpenditures (AppConfig s) = do
  dbResult <- s $ H.tx Nothing allExpenditures
  case dbResult of
    (Left err) -> do
      json $ object [ "error" .= show err ]
      status badRequest400
    (Right expenditure) -> json expenditure

getExpenditure :: RouteHandler
getExpenditure (AppConfig s) = do
  eid <- param "id"
  expenditure <- fmap flattenErr $ s $ H.tx Nothing $ singleExpenditure $ ExpenditureId eid
  case expenditure of
    (Left err) -> do
      json $ object [ "error" .= show err ]
      status notFound404
    (Right ex) ->json ex
  where flattenErr = bimap (T.pack . show) (justErr ("bla" :: T.Text))

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
