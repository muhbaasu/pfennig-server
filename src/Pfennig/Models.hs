module Models where

import           Control.Applicative ((<$>))
import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.DateTime       (DateTime)
import           Data.Text.Lazy      (Text)

newtype ExpenditureId = ExpenditureId Integer deriving (Eq, Show)

data Expenditure = Expenditure {
       _expId        :: ExpenditureId
     , _expCreatedAt :: DateTime
     , _expUpdatedAt :: DateTime
     , _expFields    :: ExpenditureFields
     } deriving Show

data ExpenditureFields = ExpenditureFields {
       _expFldDescription :: Text
     } deriving (Show)

instance ToJSON ExpenditureId where
  toJSON (ExpenditureId eid) = toJSON eid

instance ToJSON Expenditure where
  toJSON label = object [ "id" .= _expId label
                        , "createdAt" .= _expCreatedAt label
                        , "updatedAt" .= _expUpdatedAt label
                        , "description" .= desc ]
    where desc = _expFldDescription $ _expFields label

instance ToJSON ExpenditureFields where
  toJSON (ExpenditureFields desc) = object [ "description" .= desc ]

instance FromJSON ExpenditureFields where
  parseJSON (Object v) = ExpenditureFields <$> v .: "description"
  parseJSON _ = mzero