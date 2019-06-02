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
import qualified Scruffy.Data                              as SD
import qualified Scruffy.Repository                        as R
import qualified Scruffy.Service                           as S

import           Servant.Server                            as SS

import           System.Posix.Signals

type PoolReader a =
    ReaderT (Pool Connection) a

type PostgresM a =
    R.PGRepoT (PoolReader a)

type App = ServerT API (S.ServiceT (PostgresM (ExceptT SD.Error IO)))

runPGRepoT
    :: (MonadIO m, MonadError SD.Error m)
    => PostgresM m a
    -> Pool Connection
    -> m a
runPGRepoT x = runReaderT (R.runPGRepoT x)

runServiceT
    :: (MonadIO m, MonadError SD.Error m)
    => S.ServiceT (PostgresM m) a
    -> Pool Connection
    -> m a
runServiceT x = runPGRepoT (S.runServiceT x)

nt :: Pool Connection
   -> S.ServiceT (PostgresM (ExceptT SD.Error IO)) a
   -> SS.Handler a
nt pool app' =
    SS.Handler $ withExceptT SD.convertToServantErr $ runServiceT app' pool

app :: Pool Connection -> Application
app s = serve api $
    hoistServer api (nt s) (server :: App)

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
       waitForStop (do putStrLn $ "listening on " ++ show (port conf)
                       runSettings settings application)
                   (destroyAllResources pool)
