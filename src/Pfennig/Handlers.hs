{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Handlers where

import           App
import           Auth
import           Models
import           Queries                    as Q
import           View                       (index, main')

import           Control.Error.Safe         (justErr)
import           Control.Monad              (join)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (ask)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Either (left)
import           Data.Aeson                 ()
import           Data.Bifunctor             (bimap)
import           Data.ByteString.Lazy       (ByteString)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  ()
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.LocalTime        (LocalTime)
import qualified Hasql                      as H
import           Lucid                      (renderBS)
import           Network.HTTP.Types.Status
import           Servant
import           Web.Scotty                 (ActionM, header, raw, redirect,
                                             setHeader, status)

type ExpenditureAPI = "expenditure" :>
                 (    Get '[JSON] [Expenditure 'Database]
                 :<|> Capture "id" ExpenditureId :> Get '[JSON] (Expenditure 'Database)
                 :<|> Capture "start" LocalTime :> Capture "end" LocalTime :> Get '[JSON] [Expenditure 'Database]
                 :<|> ReqBody '[JSON] NewExpenditure :> Post '[JSON] (Expenditure 'Database)
                 :<|> Capture "id" ExpenditureId :> Delete '[JSON] ()
                 )

expenditureAPI :: Proxy ExpenditureAPI
expenditureAPI = Proxy

server :: ServerT ExpenditureAPI RouteM
server = getExpenditures
    :<|> getExpenditure
    :<|> getExpendituresBetween
    :<|> createExpenditure
    :<|> Handlers.deleteExpenditure

getCss :: ByteString -> ActionM ()
getCss css = do
  raw css
  setHeader "Content-Type" "text/css"
  setHeader "Cache-Control" "no-transform,public,max-age=300,s-maxage=900"
  status ok200
  return ()

getExpenditure :: ExpenditureId -> RouteM (Expenditure 'Database)
getExpenditure eid = do
  (AppConfig session)<- ask
  expenditure <- liftIO $ fmap mapErrText $ session $ H.tx Nothing $ singleExpenditure eid
  case expenditure of
    (Left _) -> lift $ left err404 -- TODO log err
    (Right ex) -> return ex
  where mapErrText = join . bimap unpackSessionError (justErr "Not found")

createExpenditure :: NewExpenditure -> RouteM (Expenditure 'Database)
createExpenditure expend = do
    (AppConfig session)<- ask
    dbResult <- liftIO $ session $ H.tx Nothing (Q.insertExpenditure (UserId 1) expend)
    case dbResult of
      (Left _) -> lift $ left err400 -- TODO log err
      (Right expenditure) -> return expenditure

getExpenditures :: RouteM [Expenditure 'Database]
getExpenditures = do
  (AppConfig session) <- ask
  dbResult <- liftIO $ session $ H.tx Nothing Q.allExpenditures
  case dbResult of
   (Left _) -> lift $ left err400
   (Right expenditures) -> return expenditures

getExpendituresBetween :: LocalTime -> LocalTime -> RouteM [Expenditure 'Database]
getExpendituresBetween start end = do
  (AppConfig session) <- ask
  res <- liftIO $ session $ H.tx Nothing $ Q.allExpendituresBetween (start, end)
  case res of
    (Left _) -> lift $ left err400
    (Right expenditures) -> return expenditures

--updateExpenditure :: RouteHandler
--updateExpenditure =
--  return $ do
--    _ <- param "id" :: ActionM Integer
--    expenditure <- jsonData :: ActionM (Expenditure 'Database)
--    json expenditure
--    status accepted202

deleteExpenditure :: ExpenditureId -> RouteM ()
deleteExpenditure eid = do
  (AppConfig session) <- ask
  count <- liftIO $ session $ H.tx Nothing $ Q.deleteExpenditure eid -- count should probably be checked
  case count of
    (Right 1) -> return ()-- accepted202
    (Right 0) -> lift $ left err404
    _ -> lift $ left err500 -- something weird happened

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
