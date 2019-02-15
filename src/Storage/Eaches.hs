{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Storage.Eaches where

import           Control.Concurrent.Async.Lifted.Safe

--
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Control          (MonadBaseControl)
import           Data.Foldable                        (traverse_)
import           Data.Int
import           Data.Pool
import           Database.Persist.Class
import           Database.Persist.Postgresql
import           Database.Persist.Quasi
import           Database.Persist.Sql
import           Database.Persist.TH
import           Database.Persist.Types

share
  [mkPersist sqlSettings]
  [persistLowerCase|
  Each
    name String
    deriving Show
 |]

data Eaches f = Eaches
  { getEach   :: Int64 -> f (Maybe Each)
  , getEaches :: f [Each]
  , saveEach  :: Each -> f ()
  }

newtype App f = App
  { runApp :: f ()
  }

makeApp ::
     (MonadIO f, Monad f, MonadBaseControl IO f, Forall (Pure f)) => Eaches (ReaderT backend f) -> Pool backend -> App f
makeApp eaches pool =
  App $
  replicateConcurrently_ 10 $
  withResource pool $
  runReaderT $ do
    saveEach eaches (Each "foo")
    e <- getEach eaches 2
    liftIO $ print e
    getEaches eaches >>= traverse_ (liftIO . print)

runReaderClassy :: MonadReader a m => ReaderT a m b -> m b
runReaderClassy = (ask >>=) . runReaderT

type ReadBackend m = MonadReader SqlBackend m

makeEaches :: (MonadIO f, ReadBackend f) => Eaches f
makeEaches =
  Eaches
    { getEach = runReaderClassy . get . toSqlKey
    , getEaches = runReaderClassy . fmap (fmap entityVal) $ selectList [] []
    , saveEach = void . runReaderClassy . insert
    }

runner :: IO ()
runner =
  runStdoutLoggingT $
  withPostgresqlPool "host=localhost port=5432 user=postgres dbname=postgres password=postgres" 10 $
  runApp . makeApp makeEaches
