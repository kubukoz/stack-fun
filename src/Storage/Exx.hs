{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Storage.Exx where

import           Control.Concurrent.Async.Lifted.Safe
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Pool
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.Sqlite
import           Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Person
    name String
    age Int Maybe
    deriving Show
  BlogPost
    title String
    authorId PersonId
    deriving Show
    |]

runner :: (MonadIO m, MonadBaseControl IO m, Forall (Pure m)) => Int -> Pool SqlBackend -> m ()
runner size pool =
  replicateConcurrently_ size $
  withResource pool $
  runReaderT $ do
    runMigration migrateAll
    johnId <- insert $ Person "John Doe" $ Just 35
    janeId <- insert $ Person "Jane Doe" Nothing
    insert $ BlogPost "My fr1st p0st" johnId
    insert $ BlogPost "One more for good measure" johnId
    oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity BlogPost])
    john <- get johnId
    liftIO $ print (john :: Maybe Person)
    delete janeId
    deleteWhere [BlogPostAuthorId ==. johnId]

main :: IO ()
main =
  runStdoutLoggingT $
  withPostgresqlPool "host=localhost port=5432 user=postgres dbname=postgres password=postgres" size $ runner size
  where
    size = 10
