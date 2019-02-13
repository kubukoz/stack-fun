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
import           Database.Persist.Sql
import           Database.Persist.TH
import           Database.Persist.Types

newtype EachId = EachId
  { eachId :: Int64
  }

newtype Each = Each
  { name :: String
  } deriving (Show)

data Eaches f = Eaches
  { getEach   :: EachId -> f (Maybe Each)
  , getEaches :: f [Each]
  , saveEach  :: Each -> f ()
  }

share
  [mkPersist sqlSettings]
  [persistLowerCase|
  PEach
    name String
    deriving Show
 |]

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
    e <- getEach eaches (EachId 2)
    liftIO $ print e
    getEaches eaches >>= traverse_ (liftIO . print)

convEach :: PEach -> Each
convEach = Each . pEachName

convPEach :: Each -> PEach
convPEach (Each name) = PEach name

runReaderClassy :: MonadReader a m => ReaderT a m b -> m b
runReaderClassy = (ask >>=) . runReaderT

type ReadBackend m = MonadReader SqlBackend m

makeEaches :: (MonadIO f, ReadBackend f) => Eaches f
makeEaches =
  Eaches
    { getEach = runReaderClassy . fmap (fmap convEach) . get . toSqlKey . eachId
    , getEaches = runReaderClassy (fmap (fmap $ convEach . entityVal) (selectList [] []))
    , saveEach = void . runReaderClassy . insert . convPEach
    }

runner :: IO ()
runner =
  runStdoutLoggingT $
  withPostgresqlPool "host=localhost port=5432 user=postgres dbname=postgres password=postgres" 10 $
  runApp . makeApp makeEaches
