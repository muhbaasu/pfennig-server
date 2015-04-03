{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           App
import           Control.Exception.Base (bracket)
import           Migrations
import           Web.Scotty             (ScottyM, delete, get, post, scotty)

import qualified Handlers
import qualified Hasql                  as H
import qualified Hasql.Postgres         as HP
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
             , S.createLabelsTable
             , S.createExpendituresLabelsTable]

setupRoutes :: AppConfig -> ScottyM ()
setupRoutes cfg = do
  get "/expenditure" $ Handlers.getExpenditures cfg
  get "/expenditure/:id" $ Handlers.getExpenditure cfg
  post "/expenditure" $ Handlers.createExpenditure cfg
  post "/expenditure/:id" $ Handlers.updateExpenditure cfg
  delete "/expenditure/:id" $ Handlers.deleteExpenditure cfg
