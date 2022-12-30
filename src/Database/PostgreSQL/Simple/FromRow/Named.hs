-- |
-- Module      : Database.PostgreSQL.Simple.FromRow.Named
-- Description : Generic implementation of FromRow based on record field names.
-- Copyright   : (c) Moritz Kiefer, 2017
-- License     : BSD-3
-- Maintainer  : moritz.kiefer@purelyfunctional.org
--
-- This module provides the machinery for implementing instances of
-- 'FromRow' that deserialize based on the names of columns instead of
-- the positions of individual fields. This is particularly convenient
-- when deserializing to a Haskell record and you want the field names
-- and column names to match up. In this case 'gFromRow' can be used as
-- a generic implementation of 'fromRow'.
module Database.PostgreSQL.Simple.FromRow.Named
  ( -- * Deserialize individual fields based on their name
    fieldByNameWith
  , fieldByName

    -- * Exception types
  , NoSuchColumn (..)
  ) where

import Control.Exception (Exception)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Database.PostgreSQL.Simple.FromField (FieldParser, FromField (fromField))
import Database.PostgreSQL.Simple.FromRow (RowParser)
import qualified Database.PostgreSQL.Simple.Internal as PostgresInternal


-- | Thrown when there is no column of the given name.
newtype NoSuchColumn = NoSuchColumn ByteString
  deriving (Show, Eq, Ord)


instance Exception NoSuchColumn


findColumn :: ByteString -> LibPQ.Result -> IO (Maybe LibPQ.Column)
findColumn columnName rowresult = do
  numberOfColumns <- LibPQ.nfields rowresult

  let columns = [LibPQ.Col 0 .. numberOfColumns - 1]

      search column prev = do
        name <- LibPQ.fname rowresult column

        if name == Just columnName
          then pure (Just column)
          else prev

  foldr search (pure Nothing) columns


-- | This is similar to 'fieldWith' but instead of trying to
-- deserialize the field at the current position it goes through all
-- fields in the current row (starting at the beginning not the
-- current position) and tries to deserialize the first field with a
-- matching column name.
fieldByNameWith :: FieldParser a -> ByteString -> RowParser a
fieldByNameWith fieldParser columnName =
  PostgresInternal.RP $ do
    PostgresInternal.Row{rowresult, row} <- ask

    maybeTargetColumn <-
      lift . lift . PostgresInternal.liftConversion $
        findColumn columnName rowresult

    case maybeTargetColumn of
      Nothing ->
        lift . lift . PostgresInternal.conversionError $
          NoSuchColumn columnName
      Just targetColumn ->
        lift . lift $ do
          oid <- PostgresInternal.liftConversion (LibPQ.ftype rowresult targetColumn)
          val <- PostgresInternal.liftConversion (LibPQ.getvalue rowresult row targetColumn)
          fieldParser (PostgresInternal.Field rowresult targetColumn oid) val


-- | This is a wrapper around 'fieldByNameWith' that gets the
-- 'FieldParser' via the typeclass instance. Take a look at the docs
-- for 'fieldByNameWith' for the details of this function.
fieldByName :: FromField a => ByteString -> RowParser a
fieldByName =
  fieldByNameWith fromField
