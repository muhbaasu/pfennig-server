{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Handlers where

import           App
import           Auth
import           Control.Error.Safe         (justErr)
import           Control.Monad              (join)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (ask, asks)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Either (left)
import           Data.Aeson                 (object, (.=))
import           Data.Bifunctor             (bimap)
import           Data.ByteString.Lazy       (ByteString)
import           Data.Maybe                 (fromMaybe)
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.LocalTime        (LocalTime)
import qualified Hasql                      as H
import           Lucid                      (renderBS)
import           Models
import           Network.HTTP.Types.Status
import           Queries                    as Q
import           Servant
import           View                       (index, main')
import           Web.Scotty                 (ActionM, header, json, jsonData,
                                             param, raw, redirect, setHeader,
                                             status)

type ExpenditureAPI = "expenditure" :> QueryParam "id" ExpenditureId :> Get '[JSON] (Expenditure 'REST)
                 :<|> "expenditure" :> NewExpenditure :> Post '[JSON] (Expenditure 'REST)
                 :<|> "expenditure" :> QueryParam "id" ExpenditureId :> Delete '[JSON] ()
                 :<|> "expenditures"
                      :> QueryParam "start" LocalTime
                      :> QueryParam "end" LocalTime
                      :> Get '[JSON] [Expenditure 'REST]

getCss :: ByteString -> ActionM ()
getCss css = do
  raw css
  setHeader "Content-Type" "text/css"
  setHeader "Cache-Control" "no-transform,public,max-age=300,s-maxage=900"
  status ok200
  return ()

getExpenditure :: ExpenditureId -> RouteM (Expenditure 'Database)
getExpenditure eid = do
  (AppConfig session) <- ask
  expenditure <- fmap mapErrText $ session $  H.tx Nothing $ singleExpenditure eid
  case expenditure of
    (Left err) -> lift $ left err404
    (Right ex) -> ex
  where mapErrText = join . bimap unpackSessionError (justErr "Not found")

createExpenditure :: RouteHandler
createExpenditure (AppConfig s) = do
    expend <- jsonData :: ActionM NewExpenditure
    dbResult <- s $ H.tx Nothing (Q.insertExpenditure (UserId 1) expend)
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

getExpendituresBetween :: RouteHandler
getExpendituresBetween (AppConfig s) = do
  start  <- param "start" :: ActionM LocalTime
  end    <- param "end" :: ActionM LocalTime
  res    <- s $ H.tx Nothing $ Q.allExpendituresBetween (start, end)
  case res of
    (Left err) -> do
      json $ object [ "error" .= show err ]
      status badRequest400
    (Right expenditure) -> json expenditure

--updateExpenditure :: RouteHandler
--updateExpenditure =
--  return $ do
--    _ <- param "id" :: ActionM Integer
--    expenditure <- jsonData :: ActionM (Expenditure 'Database)
--    json expenditure
--    status accepted202

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

main' :: RouteHandler
main' (AppConfig _) = do
  now <- liftIO getCurrentTime
  cookie <- header "Cookie"
  if fromMaybe False $ isAuthorized now <$> cookie
    then lucid $ index View.main'
    else redirect "/"
  where lucid h = do
          setHeader "Content-Type" "text/html"
          raw . renderBS $ h
