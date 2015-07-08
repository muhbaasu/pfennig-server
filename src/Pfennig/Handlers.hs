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
import           Data.Time.LocalTime       (LocalTime)
import qualified Hasql                     as H
import           Lucid                     (renderBS)
import           Network.HTTP.Types.Status
import           View                      (index, main')
import           Web.Scotty                (ActionM, header, json, jsonData,
                                            param, raw, redirect, setHeader,
                                            status)

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
    exp <- jsonData :: ActionM NewExpenditure
    dbResult <- s $ H.tx Nothing (Q.insertExpenditure (UserId 1) exp)
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
  state <- header "Cookie"
  case state of
   Nothing -> redirect "/"
   Just _ -> lucid $ index View.main'
  where lucid h = do
          setHeader "Content-Type" "text/html"
          raw . renderBS $ h
