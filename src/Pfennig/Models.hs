{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Scientific     (Scientific)
import           Data.Text           (Text)
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime (LocalTime)

data Reference = -- | Might still require joins
                 Database
                 -- | URI
                 | REST
                 -- | Fully materialized object
                 | NoRef

type family RefType (a :: *) (r :: Reference) :: * where
  RefType (Expenditure r) Database = ExpenditureId
  RefType User Database = UserId
  RefType a NoRef = a

newtype ExpenditureId = ExpenditureId Int deriving (Eq, Show)
newtype ExpenditureTagId = ExpenditureTagId Int deriving (Eq, Show)
newtype UserId = UserId Int deriving (Eq, Show)

-- | batch of expenses at a specific point in time
data Expenditure (r :: Reference)=
  Expenditure {
      _expId        :: ExpenditureId
    , _expCreatedAt :: LocalTime
    , _expUpdatedAt :: LocalTime
    , _expName      :: Text
    , _expAmount    :: Scientific
    , _expUser      :: RefType User r
    , _expFields    :: ExpenditureFields r
    }

-- | a single expense description e.g. "cellphone bill"
data ExpenditureFields (r :: Reference) =
  ExpenditureFields {
      _expFldDescription :: Text
    , _expFldTags        :: [ExpenditureTag r]
    } deriving (Show)

-- | a tag to group the expenditures by
data ExpenditureTag (r :: Reference) =
  ExpenditureTag {
       _tagId        :: ExpenditureTagId
     , _tagTitle     :: Text
     , _tagCreatedAt :: LocalTime
     } deriving (Show)

data User =
  User {
    _usrId        :: UserId
  , _usrCreatedAt :: LocalTime
  , _usrUpdatedAt :: LocalTime
  , _usrLogin     :: Text
  , _usrEmail     :: Text
  } deriving (Show)

instance ToJSON ExpenditureId where
  toJSON (ExpenditureId eid) = toJSON eid

instance ToJSON UserId where
  toJSON (UserId uid) = toJSON uid

instance ToJSON (Expenditure r) where
  toJSON label = object [ "id"          .= _expId label
                        , "createdAt"   .= _expCreatedAt label
                        , "updatedAt"   .= _expUpdatedAt label
                        , "name"        .= _expName label
                        , "amount"      .= _expAmount label
                        , "description" .= desc ]
 --                     , "user"        .= _expUser label
     where desc = _expFldDescription $ _expFields label

instance ToJSON (ExpenditureFields r) where
  toJSON (ExpenditureFields desc tags) = object [ "description" .= desc ]

instance FromJSON (ExpenditureFields r) where
  parseJSON (Object v) = ExpenditureFields <$> v .: "description" <*> pure []
  parseJSON _ = mzero

instance ToJSON User where
  toJSON user = object [ "id"        .= _usrId user
                       , "createdAt" .= _usrCreatedAt user
                       , "updatedAt" .= _usrUpdatedAt user
                       , "login"     .= _usrLogin user
                       , "email"     .= _usrEmail user ]

instance ToJSON LocalTime where
  toJSON = toJSON . formatTime defaultTimeLocale "%Y%m%dT%H%M%S"
