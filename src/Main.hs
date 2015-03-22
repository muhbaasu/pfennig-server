{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Exception.Base (bracket)
import           Control.Lens.TH        (makeClassy)
import           Data.DateTime          (DateTime)
import           Migrations

import qualified Hasql                  as H
import qualified Hasql.Postgres         as HP
import qualified Schema                 as S

newtype ExpenditureId = ExpenditureId Int deriving (Eq, Show)
newtype ExpenditureLabelId = ExpenditureLabelId Int deriving (Eq, Show)

data ExpenditureFields = ExpenditureFields { _expenditureDescription :: String
                                           } deriving Show

data Expenditure = Expenditure { _expenditureId        :: ExpenditureId
                               , _expenditureCreatedAt :: DateTime
                               , _expenditureUpdatedAt :: DateTime
                               , _expenditureFields    :: ExpenditureFields
                               } deriving Show

data ExpenditureLabel = ExpenditureLabel { _expenditureLabelId        :: ExpenditureLabelId
                                         , _expenditureLabelCreatedAt :: DateTime
                                         , _expenditureLabelTitle     :: String
                                         } deriving Show

makeClassy ''ExpenditureFields

main :: IO ()
main = do
  let postgresSettings = HP.ParamSettings
                         "localhost"
                         5432
                         "pfennig"
                         "pfennig"
                         "pfennig"
  sessionSettings <- maybe (fail "Invalid settings") return $ H.poolSettings 6 30

  bracket
    (H.acquirePool postgresSettings sessionSettings)
    H.releasePool
    (runMigrations migrations)

migrations :: [H.Stmt HP.Postgres]
migrations = [ S.createExpendituresTable
             , S.createLabelsTable
             , S.createExpendituresLabelsTable]
