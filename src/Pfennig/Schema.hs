{-# LANGUAGE QuasiQuotes #-}

module Schema where

import qualified Hasql          as H
import qualified Hasql.Postgres as HP

createExpendituresTable :: H.Stmt c
createExpendituresTable =
  [H.stmt|create table expenditures
  ( id serial primary key
  , created_at timestamp with time zone not null default now()
  , updated_at timestamp with time zone not null default now()
  )|]

createExpenditureFieldsTable :: H.Stmt c
createExpenditureFieldsTable =
  [H.stmt|create table expenditure_fields
  ( id serial primary key
  , created_at timestamp with time zone not null default now()
  , updated_at timestamp with time zone not null default now()
  )|]

createLabelsTable :: H.Stmt c
createLabelsTable =
  [H.stmt|create table labels
  ( id serial primary key
  , title varchar (40)
  , created_at timestamp with time zone not null default now()
  )|]

-- |
-- Create table which maps labels to expenditures.
createExpendituresLabelsTable :: H.Stmt c
createExpendituresLabelsTable =
  [H.stmt|create table expenditures_labels
  ( id serial primary key
  , expenditure_id serial references expenditures
  , label_id serial references labels
  , created_at timestamp with time zone not null default now()
  )|]
