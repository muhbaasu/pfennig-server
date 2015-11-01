{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module App where

import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.Trans.Either (EitherT)
import qualified Hasql                      as H
import qualified Hasql.Postgres             as HP
import           Servant
import           Web.Scotty                 (ActionM)

type RouteHandler = AppConfig -> ActionM ()

type RouteM =  ReaderT AppConfig (EitherT ServantErr IO)

data AppConfig = AppConfig {
    dbSession :: forall m a. H.Session HP.Postgres m a
                 -> m (Either (H.SessionError HP.Postgres) a) }
