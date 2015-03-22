{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Migrations where

import           Control.Applicative   ((<$>))
import           Control.Monad         (forM_)
import           Data.ByteString.Lazy  (fromStrict)
import           Data.Digest.Pure.SHA  (sha1, showDigest)
import           Data.Functor.Identity (Identity (), runIdentity)
import           Data.Maybe            (maybeToList)
import           Safe                  (lastMay)

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Data.Traversable      as TR
import qualified Hasql                 as H
import qualified Hasql.Backend         as HB
import qualified Hasql.Postgres        as HP

data Migration = Migration { _migrationName     :: T.Text
                           , _migrationTx       :: forall s. H.Tx HP.Postgres s ()
                           , _migrationChecksum :: Checksum }

instance Show Migration where
  show (Migration name _ checksum) = "Migration " ++ show name ++
                                     " (" ++ show checksum ++ ")"

migration :: T.Text -> [H.Stmt HP.Postgres] -> Migration
migration name stats = Migration name (createTx stats) (calculateChecksum $ head stats)
  where createTx ss  = undefined
        calculateChecksum ss = map computeChecksum ss

type Checksum = T.Text

readMode :: H.TxWriteMode
readMode = Nothing

writeMode :: H.TxWriteMode
writeMode = Just True

writeModeWithRollback :: H.TxWriteMode
writeModeWithRollback = Just False

migrationState :: H.Stmt c
migrationState = [H.stmt| select checksum, updated_at from schema_migrations |]

saveMigrationState :: Checksum -> H.Tx HP.Postgres s ()
saveMigrationState cs =  H.unitEx $ [H.stmt| insert into schema_migrations
                                             (checksum) values (?) |] cs

data SchemaState = SchemaState { _schemaStateChecksum  :: Checksum
                               , _schemaStateUpdatedAt :: Int }

currentSchemaVersion :: H.Tx HP.Postgres s (Maybe SchemaState)
currentSchemaVersion = do
  current :: Maybe (Checksum, Int) <- H.maybeEx migrationState
  return $ uncurry SchemaState <$> current

migrate :: [H.Stmt HP.Postgres] -> H.Tx HP.Postgres s ()
migrate ms = do
  _ <- createMigrationTable
  schema <- currentSchemaVersion
  let migs = computeMigrations (_schemaStateChecksum <$> schema) ms
  _ <- forM_ migs H.unitEx
  _ <- TR.mapM (saveMigrationState . computeChecksum) (lastMay migs)
  return ()

runMigrations :: [H.Stmt HP.Postgres] -> H.Pool HP.Postgres -> IO ()
runMigrations ms pool = do
  result <- H.session pool $
            H.tx (Just (HB.Serializable, writeMode)) $ migrate ms
  print $ case result of
            Left e -> show e
            Right _ -> "Migrated successfully!"

computeMigrations :: Maybe Checksum -> [H.Stmt c] -> [H.Stmt c]
computeMigrations Nothing ms      = ms
computeMigrations (Just _) []     = []
computeMigrations cs@(Just checksum) (m:ms)
  | checksum /= computeChecksum m = computeMigrations cs ms
  | otherwise                     = ms

computeChecksum :: H.Stmt c -> Checksum
computeChecksum = hashToText . sqlToText
  where sqlToText  = fromStrict . TE.encodeUtf8 . HB.stmtTemplate
        hashToText = T.pack . showDigest . sha1

createMigrationTable :: H.Tx HP.Postgres c ()
createMigrationTable = H.unitEx $
  [H.stmt|create table if not exists schema_migrations
          ( checksum varchar(40) not null
          , updated_at timestamp without time zone not null default now()
          )|]
