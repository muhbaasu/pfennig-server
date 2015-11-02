{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           App
import qualified Auth
import           Control.Exception.Base        (bracket)

import           Control.Monad.Reader          (runReaderT)
import           Control.Monad.Trans.Either    (EitherT)
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
import           Web.Scotty                    (ActionM, ScottyM, get,
                                                middleware, raw, setHeader)

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
        run 3000 $ app cfg)

app :: AppConfig -> Application
app cfg = serve publicAPI (readerServer cfg)

server :: ServerT PublicAPI RouteM
server = Auth.server :<|> Handlers.server

type PublicAPI =  Auth.AuthAPI
                  :<|> Handlers.ExpenditureAPI

publicAPI :: Proxy PublicAPI
publicAPI = Proxy

readerToEither :: AppConfig -> RouteM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: AppConfig -> Server PublicAPI
readerServer cfg = enter (readerToEither cfg) server

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
