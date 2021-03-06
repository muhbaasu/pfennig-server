{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Queries where

import           Models

import           Data.Scientific     (Scientific)
import qualified Data.Text           as T
import           Data.Text.Encoding  (decodeUtf8)
import           Data.Time.LocalTime (LocalTime)
import           Data.Word           (Word64)
import qualified Hasql               as H
import qualified Hasql.Postgres      as HP


rowToExpenditure :: (Int, LocalTime, LocalTime, Int, T.Text, Scientific) -> Expenditure 'Database
rowToExpenditure (id', created, updated, userId, name, amount) =
  let eid = ExpenditureId id'
--      fields = ExpenditureFields name []
      user = UserId userId
  in Expenditure eid created updated name amount user -- fields

unpackCxError :: HP.CxError -> T.Text
unpackCxError (HP.CantConnect err) = maybe (T.pack "Unable to connect") decodeUtf8 err
unpackCxError (HP.UnsupportedVersion ver) = T.pack $ "Postgres version " ++ show ver ++ " is not supported."

unpackTxError :: HP.TxError -> T.Text
unpackTxError _ = T.pack "Transaction error"

unpackSessionError :: H.SessionError HP.Postgres -> T.Text
unpackSessionError (H.CxError err) = unpackCxError err
unpackSessionError (H.TxError err) = unpackTxError err
unpackSessionError (H.ResultError txt) = txt

insertExpenditure :: UserId -> NewExpenditure
                   -> H.Tx HP.Postgres s (Expenditure 'Database)
insertExpenditure (UserId uid) (NewExpenditure name amount) = do
  row <-
    H.singleEx $ [H.stmt|insert into expenditures (user_id, name, amount)
                         values (?, ?, ?) returning *|] uid name amount
  return $ rowToExpenditure row

singleExpenditure :: ExpenditureId -> H.Tx HP.Postgres s (Maybe (Expenditure 'Database))
singleExpenditure (ExpenditureId eid) = do
  row <- H.maybeEx $ [H.stmt|select * from expenditures where id = ?|] eid
  return $ fmap rowToExpenditure row

allExpenditures :: H.Tx HP.Postgres s [Expenditure 'Database]
allExpenditures = do
  rows <- H.listEx $ [H.stmt|select * from expenditures|]
  return $ fmap rowToExpenditure rows

deleteExpenditure :: ExpenditureId -> H.Tx HP.Postgres s Word64
deleteExpenditure (ExpenditureId eid) =
  H.countEx $ [H.stmt|delete from expenditures where id = ?|] eid

type TimeRange = (LocalTime, LocalTime)
allExpendituresBetween :: TimeRange -> H.Tx HP.Postgres s [Expenditure 'Database]
allExpendituresBetween (start, end) = do
  rows <- H.listEx $ [H.stmt|select * from expenditures
                             where (created_at, created_at)
                             overlaps (?, ?) |] start end
  return $ fmap rowToExpenditure rows
