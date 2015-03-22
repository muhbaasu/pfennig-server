{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Exception.Base (bracket)
import           Control.Lens.TH        (makeClassy)
import           Data.Aeson             (ToJSON)
import           Data.DateTime          (DateTime, fromSeconds)
import           GHC.Generics
import           Migrations
import           Web.Scotty             (addHeader, get, json, scotty)

import qualified Hasql                  as H
import qualified Hasql.Postgres         as HP
import qualified Schema                 as S

newtype ExpenditureId = ExpenditureId Int deriving (Eq, Show, Generic)
newtype ExpenditureLabelId = ExpenditureLabelId Int deriving (Eq, Show, Generic)

data ExpenditureFields = ExpenditureFields { _expenditureDescription :: String
                                           } deriving (Show, Generic)

data Expenditure = Expenditure { _expenditureId        :: ExpenditureId
                               , _expenditureCreatedAt :: DateTime
                               , _expenditureUpdatedAt :: DateTime
                               , _expenditureFields    :: ExpenditureFields
                               } deriving (Show, Generic)

data ExpenditureLabel = ExpenditureLabel { _expenditureLabelId        :: ExpenditureLabelId
                                         , _expenditureLabelCreatedAt :: DateTime
                                         , _expenditureLabelTitle     :: String
                                         } deriving (Show, Generic)
instance ToJSON ExpenditureLabelId
instance ToJSON ExpenditureId
instance ToJSON ExpenditureLabel
instance ToJSON ExpenditureFields
instance ToJSON Expenditure


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

  runScotty

migrations :: [H.Stmt HP.Postgres]
migrations = [ S.createExpendituresTable
             , S.createLabelsTable
             , S.createExpendituresLabelsTable]

runScotty :: IO ()
runScotty =
  scotty 3000 $
    get "/expenditure" $ do
      json [Expenditure eid created updated field]
      addHeader "Access-Control-Allow-Origin" "*"
      where eid = ExpenditureId 0
            created = fromSeconds 0
            updated = fromSeconds 100
            field = ExpenditureFields "description"
