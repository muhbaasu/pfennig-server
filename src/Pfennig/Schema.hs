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

createTagsTable :: H.Stmt c
createTagsTable =
  [H.stmt|create table tags
  ( id serial primary key
  , ti3tle varchar (40)
  , created_at timestamp with time zone not null default now()
  )|]

-- |
-- Create table which maps tags to expenditures.
createExpendituresTagsTable :: H.Stmt c
createExpendituresTagsTable =
  [H.stmt|create table expenditures_tags
  ( id serial primary key
  , expenditure_id serial references expenditures
  , tags_id serial references tags
  , created_at timestamp with time zone not null default now()
  )|]
