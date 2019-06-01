{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import           Config

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Default
import           Data.Pool

import           Database.PostgreSQL.Simple

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON

import           Scruffy.API
import qualified Scruffy.Repository                        as R
import qualified Scruffy.Service                           as S

import           Servant.Server                            as SS

import           System.Posix.Signals

runPGRepoT :: MonadIO m => R.PGRepoT m a -> Pool Connection -> m a
runPGRepoT x = runReaderT (R.runPGRepoT x)

runServiceT
    :: (MonadIO m) => S.ServiceT (R.PGRepoT m) a -> Pool Connection -> m a
runServiceT x = runPGRepoT (S.runServiceT x)

nt :: Pool Connection -> AppM a -> SS.Handler a
nt pool app' = SS.Handler $ ExceptT $
    runServiceT (runExceptT $ runAppT app') pool

newtype AppM a =
    AppM { runAppT :: ExceptT ServantErr (S.ServiceT (R.PGRepoT IO)) a }
    deriving ( Functor, Applicative, Monad, MonadError ServantErr )

instance S.Service AppM where
    searchAlbums r = AppM $
        ExceptT (Right <$> S.searchAlbums r)

    searchBands r = AppM $
        ExceptT (Right <$> S.searchBands r)

    getBand v = AppM $ ExceptT (Right <$> S.getBand v)

app :: Pool Connection -> Application
app s = serve api $
    hoistServer api (nt s) (server :: ServerT API AppM)

waitForStop :: IO b -> IO a -> IO ()
waitForStop ioAction cleanup =
    do stopSignal <- newEmptyMVar
       let handler = CatchOnce $ cleanup >>= putMVar stopSignal
       _ <- installHandler sigINT handler Nothing
       race_ (takeMVar stopSignal) ioAction

main :: IO ()
main =
    do conf <- readConf
       pool <- R.connectPGPool $ unConfig $ database conf
       logger <- mkRequestLogger $
           def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }
       let settings = setPort (port conf) defaultSettings
       let application = logger $ app pool
       waitForStop (runSettings settings application)
                   (destroyAllResources  pool)
