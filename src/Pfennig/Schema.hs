{-# LANGUAGE QuasiQuotes #-}

module Schema where

import qualified Hasql as H

createUsers :: H.Stmt c
createUsers =
  [H.stmt|
   create table users (
     id serial primary key,
     created_at timestamp with time zone not null default now(),
     updated_at timestamp with time zone not null default now(),
     email text not null,
     password bytea not null)
   |]

createIntervals :: H.Stmt c
createIntervals =
  [H.stmt|
   create table intervals (
     id serial primary key,
     created_at timestamp with time zone not null default now(),
     updated_at timestamp with time zone not null default now(),
     name text not null)
   |]

createExpenditures :: H.Stmt c
createExpenditures =
  [H.stmt|
   create table expenditures (
     id serial primary key,
     created_at timestamp with time zone not null default now(),
     updated_at timestamp with time zone not null default now(),
     user_id integer not null references users (id),
     name text not null,
     amount numeric(10, 5) not null check (amount >= 0))
   |]

createTags :: H.Stmt c
createTags =
  [H.stmt|
   create table tags (
     id serial primary key,
     created_at timestamp with time zone not null default now(),
     updated_at timestamp with time zone not null default now(),
     user_id integer not null references users (id),
     name text not null)
   |]

createExpendituresTags :: H.Stmt c
createExpendituresTags =
  [H.stmt|
   create table expenditures_tags (
     id serial primary key,
     created_at timestamp with time zone not null default now(),
     updated_at timestamp with time zone not null default now(),
     expenditure_id integer not null references expenditures (id),
     tag_id integer not null references tags (id),
     unique (expenditure_id, tag_id))
   |]

createExpendituresIntervals :: H.Stmt c
createExpendituresIntervals =
  [H.stmt|
   create table expenditures_intervals (
     id serial primary key,
     created_at timestamp with time zone not null default now(),
     updated_at timestamp with time zone not null default now(),
     expenditure_id integer not null unique references expenditures (id),
     interval_id integer not null references intervals (id),
     unit numeric(10, 5) not null check (unit >= 0))
   |]
