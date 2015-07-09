{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           App
import           Auth
import           Control.Exception.Base        (bracket)
import qualified Handlers
import qualified Hasql                         as H
import qualified Hasql.Postgres                as HP
import           Layout                        (readCSS)
import           Lucid
import           Migrations
import           Network.Wai.Middleware.Static (CacheContainer, CachingStrategy (PublicStaticCaching),
                                                hasPrefix, initCaching,
                                                staticPolicy')
import qualified Schema                        as S
import           View
import           Web.Scotty                    (ActionM, ScottyM, delete, get,
                                                header, middleware, post, raw,
                                                redirect, scotty, setHeader)

main :: IO ()
main = do
  let postgresSettings = HP.ParamSettings
                         "localhost"
                         5432
                         "pfennig"
                         "pfennig"
                         "pfennig"
  sessionSettings <- maybe (fail "Invalid settings") return $ H.poolSettings 6 3
  cache <- initCaching PublicStaticCaching

  bracket
    (H.acquirePool postgresSettings sessionSettings)
    H.releasePool
    (\pool -> do
        let session = H.session pool
        let cfg = AppConfig session
        runMigrations migrations pool
        scotty 3000 $ do
          setupMiddleware cache
          setupAssets
          setupViewRoutes
          setupAPIRoutes cfg)

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
setupMiddleware cache = do
  middleware $ staticPolicy' cache $ hasPrefix "assets/"

setupAssets :: ScottyM ()
setupAssets = do
  get "/assets/generated.css" $ Handlers.getCss readCSS

setupViewRoutes :: ScottyM ()
setupViewRoutes = do
  get "/" $ do
    loggedIn <- (fmap . fmap) Handlers.isAuthorized $ header "Cookie"
    if maybe False id loggedIn
      then redirect "/main"
      else lucid $ index View.login
  get "/register" $ lucid $ index View.register

setupAPIRoutes :: AppConfig -> ScottyM ()
setupAPIRoutes cfg = do
 -- expenditures
  get "/expenditure" $ Handlers.getExpenditures cfg
  get "/expenditure/:id" $ Handlers.getExpenditure cfg
  get "/expenditure/:start/:end" $ Handlers.getExpendituresBetween cfg
  post "/expenditure" $ Handlers.createExpenditure cfg
  -- post "/expenditure/:id" $ Handlers.updateExpenditure cfg
  delete "/expenditure/:id" $ Handlers.deleteExpenditure cfg
  -- auth
  post "/registration" $ Auth.register
  post "/login" $ Auth.login
  get "/logout" $ do
    setHeader "Set-Cookie" "authorized=false; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT"
    redirect "/"

  get "/main" $ Handlers.main' cfg
