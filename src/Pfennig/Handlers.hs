{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers where

import           App
import           Control.Error.Safe        (justErr)
import           Control.Monad             (join)
import           Data.Aeson                (object, (.=))
import           Data.Aeson                (object, (.=))
import           Data.Bifunctor            (bimap)
import qualified Data.Text                 as T
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8)
import           Data.Time.Format          (defaultTimeLocale, parseTimeOrError)
import           Data.Time.LocalTime       (LocalTime)
import           Data.Word                 (Word64)
import qualified Hasql                     as H
import qualified Hasql.Postgres            as HP
import           Models
import           Network.HTTP.Types.Status
import           Web.Scotty                (ActionM, json, jsonData, param,
                                            status)

-- <<<<<<< HEAD
-- unpackCxError :: HP.CxError -> T.Text
-- unpackCxError (HP.CantConnect err) = maybe (T.pack "Unable to connect") decodeUtf8 err
-- unpackCxError (HP.UnsupportedVersion ver) = T.pack $ "Postgres version " ++ show ver ++ " is not supported."

-- unpackTxError :: HP.TxError -> T.Text
-- unpackTxError _ = T.pack "Transaction error"

-- unpackSessionError :: H.SessionError HP.Postgres -> T.Text
-- unpackSessionError (H.CxError err) = unpackCxError err
-- unpackSessionError (H.TxError err) = unpackTxError err
-- unpackSessionError (H.ResultError txt) = txt

-- insertExpenditure :: ExpenditureFields -> H.Tx HP.Postgres s Expenditure
-- insertExpenditure (ExpenditureFields description) = do
-- =======
-- insertExpenditure :: (ExpenditureFields 'Database)
--                   -> H.Tx HP.Postgres s (Expenditure 'Database)
-- insertExpenditure (ExpenditureFields description _) = do
-- >>>>>>> Add basic implementation of `DataKind'ed refs
--   row <-
--     H.singleEx $ [H.stmt|insert into expenditures (description)
--                          values (?) returning *|] description
--   return $ rowToExpenditure row

-- allExpenditures :: H.Tx HP.Postgres s [Expenditure 'Database]
-- allExpenditures = do
--   rows <-
--     H.listEx $ [H.stmt|select * from expenditures|]
--   return $ fmap rowToExpenditure rows

-- <<<<<<< HEAD
-- singleExpenditure :: ExpenditureId -> H.Tx HP.Postgres s (Maybe Expenditure)
-- singleExpenditure (ExpenditureId eid) = do
--   row <-
--      H.maybeEx $ [H.stmt|select * from expenditures where id = ?|] eid
--   return $ fmap rowToExpenditure row

-- delExpenditure :: ExpenditureId -> H.Tx HP.Postgres s Word64
-- delExpenditure (ExpenditureId eid) = do
--   count <-
--           H.countEx $ [H.stmt|delete from expenditures where id = ?|] eid
--   return count


-- rowToExpenditure :: (Int, LocalTime, LocalTime, T.Text) -> Expenditure
-- =======
-- rowToExpenditure :: (Int, LocalTime, LocalTime, T.Text) -> Expenditure 'Database
-- >>>>>>> Add basic implementation of `DataKind'ed refs
-- rowToExpenditure (id', created, updated, desc) =
--   let eid = ExpenditureId id'
--       fields = ExpenditureFields desc []
--   in Expenditure eid created updated fields

-- getExpenditures :: RouteHandler
-- getExpenditures (AppConfig s) = do
--   dbResult <- s $ H.tx Nothing allExpenditures
--   case dbResult of
--     (Left err) -> do
--       json $ object [ "error" .= show err ]
--       status badRequest400
--     (Right expenditure) -> json expenditure

-- getExpenditure :: RouteHandler
-- <<<<<<< HEAD
-- getExpenditure (AppConfig s) = do
--   eid <- param "id"
--   expenditure <- fmap mapErrText $ s $ H.tx Nothing $ singleExpenditure $ ExpenditureId eid
--   case expenditure of
--     (Left err) -> do
--       json $ object [ "error" .= show err ]
--       status notFound404
--     (Right ex) ->json ex
--   where mapErrText = join . bimap unpackSessionError (justErr "Not found")
-- =======
-- getExpenditure =
--   return $ do
--     eid <- param "id"
--     json $ Expenditure (ExpenditureId eid) randomLocalTime randomLocalTime fields
--   where fields = ExpenditureFields "Hello!" []
-- >>>>>>> Add basic implementation of `DataKind'ed refs

-- createExpenditure :: RouteHandler
-- createExpenditure (AppConfig s) = do
--     fields <- jsonData :: ActionM (ExpenditureFields 'Database)
--     dbResult <- s $ H.tx Nothing (insertExpenditure fields)
--     case dbResult of
--       (Left err) -> do
--         json $ object [ "error" .= show err ]
--         status badRequest400
--       (Right expenditure) -> json expenditure

-- updateExpenditure :: RouteHandler
-- updateExpenditure =
--   return $ do
--     _ <- param "id" :: ActionM Integer
--     expenditure <- jsonData :: ActionM (ExpenditureFields 'Database)
--     json expenditure
--     status accepted202

-- deleteExpenditure :: RouteHandler
-- deleteExpenditure (AppConfig s) = do
--   eid <- param "id" :: ActionM Int
--   count <- s $ H.tx Nothing $ delExpenditure $ ExpenditureId eid -- count should probably be checked
--   case count of
--     (Right 1) -> status accepted202
--     (Right 0) -> status notFound404
--     (Left err) -> do
--       json $ object [ "error" .= show err ]
--       status badRequest400
--     _ -> status internalServerError500 -- something weird happened
