module CatDb
  ( withPool
  ) where

import Control.Exception (bracket, throwIO)
import Control.Monad (void)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Postgres
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.Postgres.Temp as Temp


migrate :: Postgres.Connection -> IO ()
migrate connection =
  Postgres.withTransaction connection $ do
    let execute = void . Postgres.execute_ connection

    execute
      [sql|
        CREATE TABLE public.cats (
          id BIGSERIAL NOT NULL PRIMARY KEY
          ,name TEXT NOT NULL UNIQUE
          ,color TEXT NOT NULL
          ,personality TEXT NOT NULL
          ,date_of_birth DATE NOT NULL
          ,created_at TIMESTAMPTZ NOT NULL DEFAULT now()
          ,updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
        );
      |]

    execute
      [sql|
        INSERT INTO public.cats (
          name
          ,color
          ,personality
          ,date_of_birth
        )
        VALUES (
          'Waymond'
          ,'grey'
          ,'jock'
          ,'2020-01-02'
        ), (
          'Mochi'
          ,'orange'
          ,'peppy'
          ,'2019-02-03'
        ), (
          'Sarabi'
          ,'white'
          ,'snooty'
          ,'2018-03-04'
        );
      |]


withPool :: (Pool Postgres.Connection -> IO a) -> IO a
withPool use = do
  result <- Temp.with $ \db -> do
    let connectionString = Temp.toConnectionString db

        config =
          Pool.PoolConfig
            { createResource = Postgres.connectPostgreSQL connectionString
            , freeResource = Postgres.close
            , poolCacheTTL = 1
            , poolMaxResources = 4
            }

    bracket (Pool.newPool config) Pool.destroyAllResources $ \connections -> do
      Pool.withResource connections migrate
      use connections

  case result of
    Left ex -> throwIO ex
    Right a -> pure a
