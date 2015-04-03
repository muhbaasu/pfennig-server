{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           App
import           Control.Exception.Base (bracket)
import           Control.Monad.Reader   (runReader)
import           Migrations
import           Web.Scotty             (delete, get, post, scotty)

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
        runScotty cfg)

migrations :: [H.Stmt HP.Postgres]
migrations = [ S.createExpendituresTable
             , S.createLabelsTable
             , S.createExpendituresLabelsTable]

runScotty :: AppConfig -> IO ()
runScotty cfg =
  scotty 3000 $ do
    get "/expenditure" $ runReader Handlers.getExpenditures cfg
    get "/expenditure/:id" $ runReader Handlers.getExpenditure cfg
    post "/expenditure" $ runReader Handlers.createExpenditure cfg
    post "/expenditure/:id" $ runReader Handlers.updateExpenditure cfg
    delete "/expenditure/:id" $ runReader Handlers.deleteExpenditure cfg
