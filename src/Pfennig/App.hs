{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

import           Control.Monad.Reader
import qualified Hasql                as H
import qualified Hasql.Postgres       as HP
import           Web.Scotty           (ActionM)

type RouteHandler = Reader AppConfig (ActionM ())

data AppConfig = AppConfig {
    dbSession :: forall m a. H.Session HP.Postgres m a
                 -> m (Either (H.SessionError HP.Postgres) a) }