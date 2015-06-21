{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           App
import           Control.Exception.Base (bracket)
import           Migrations
import           View
import           Web.Scotty             (ActionM, ScottyM, delete, get, post,
                                         raw, scotty, setHeader)

import qualified Handlers
import qualified Hasql                  as H
import qualified Hasql.Postgres         as HP
import           Lucid
import qualified Schema                 as S

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
        scotty 3000 $ setupRoutes cfg)

migrations :: [H.Stmt HP.Postgres]
migrations = [ S.createExpendituresTable
             , S.createTagsTable
             , S.createExpendituresTagsTable]

lucid :: Html a -> ActionM ()
lucid h = do
  setHeader "Content-Type" "text/html"
  raw . renderBS  $ h

setupRoutes :: AppConfig -> ScottyM ()
setupRoutes cfg = do
  get "/" $ lucid index
  get "/expenditure" $ Handlers.getExpenditures cfg
  get "/expenditure/:id" $ Handlers.getExpenditure cfg
  post "/expenditure" $ Handlers.createExpenditure cfg
  post "/expenditure/:id" $ Handlers.updateExpenditure cfg
  delete "/expenditure/:id" $ Handlers.deleteExpenditure cfg
