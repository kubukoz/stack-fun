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
--
--import           Control.Monad.IO.Class      (liftIO)
--import           Database.Persist.Postgresql hiding (get)
--import           Database.Persist.TH

newtype Each =
  Each String
  deriving (Show)

newtype EachId =
  EachId Int

data Eaches f = Eaches
  { get  :: EachId -> f (Maybe Each)
  , save :: Each -> f ()
  }
--
--share
--  [mkPersist sqlSettings, mkSave "entityDefs"]
--  [persistLowerCase|
--PEach
--    name String
--    age Int Maybe
--    deriving Show
-- |]

newtype App f = App
  { run :: f ()
  }

class Console f where
  consoleOut :: Show a => a -> f ()

instance Console IO where
  consoleOut = print

makeApp :: (Console f, Monad f) => Eaches f -> App f
makeApp eaches =
  App $ do
    save eaches (Each "foo")
    e <- get eaches (EachId 1)
    consoleOut e

--fun :: (Monad f, Console f) => f ()
--fun =
--  let eaches = Eaches (const . return $ Nothing) (const . pure $ ())
--      app = makeApp eaches
--   in run app
--fun :: IO ()

