{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           App
import qualified Auth
import           Control.Exception.Base        (bracket)

import qualified Handlers
import qualified Hasql                         as H
import qualified Hasql.Postgres                as HP
import           Layout                        (readCSS)
import           Lucid
import           Migrations
import           Network.Wai                   (Application)
import           Network.Wai.Handler.Warp      (run)
import           Network.Wai.Middleware.Static (CacheContainer, hasPrefix,
                                                staticPolicy')
import qualified Schema                        as S
import           Servant
import qualified View
import           Web.Scotty                    (ActionM, ScottyM, get,
                                                middleware, notFound, raw,
                                                setHeader)

main :: IO ()
main = do
  let postgresSettings = HP.ParamSettings
                         "localhost"
                         5432
                         "pfennig"
                         "pfennig"
                         "pfennig"
  sessionSettings <- maybe (fail "Invalid settings") return $ H.poolSettings 6 3

  bracket
    (H.acquirePool postgresSettings sessionSettings)
    H.releasePool
    (\pool -> do
        let session = H.session pool
        let cfg = AppConfig session
        runMigrations migrations pool
        run 3000 app)

app :: Application
app = serve publicAPI server

server :: Server PublicAPI
server = Auth.server

type PublicAPI = Auth.AuthAPI -- :<|> Handlers.ExpenditureAPI

publicAPI :: Proxy PublicAPI
publicAPI = Proxy

migrations :: [H.Stmt HP.Postgres]
migrations = [ S.createUsers
             , S.createIntervals
             , S.createExpenditures
             , S.createTags
             , S.createExpendituresTags
             , S.createExpendituresIntervals]

lucid :: Html a -> ActionM ()
lucid h = do
  setHeader "Content-Type" "text/html"
  raw . renderBS  $ h

setupMiddleware :: CacheContainer -> ScottyM ()
setupMiddleware cache =
  middleware $ staticPolicy' cache $ hasPrefix "assets/"

setupAssets :: ScottyM ()
setupAssets =
  get "/assets/generated.css" $ Handlers.getCss readCSS

setupAPIRoutes :: AppConfig -> ScottyM ()
setupAPIRoutes cfg = do
 -- expenditures
  get "/expenditure/:id" $ Handlers.getExpenditure cfg
  get "/expenditure/:start/:end" $ Handlers.getExpendituresBetween cfg

  notFound $ lucid $ View.index View.notFound
