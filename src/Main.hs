{-# LANGUAGE OverloadedLists #-}

module Main where

import           Config

import           Control.Concurrent.Async
import           Control.Concurrent.MVar

import           Data.Default
import           Data.Pool

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON

import           Scruffy.API
import           Scruffy.Repository                        as R
import           Scruffy.Service

import           Servant.Server

import           System.Posix.Signals

waitForStop :: IO b -> IO a -> IO ()
waitForStop action cleanup =
    do stopSignal <- newEmptyMVar
       let handler = CatchOnce $ cleanup >>= putMVar stopSignal
       _ <- installHandler sigINT handler Nothing
       race_ (takeMVar stopSignal) action

main :: IO ()
main =
    do conf <- readConf
       repo <- connectPGRepo $ unConfig $ database conf
       let svc = BasicService repo
       logger <- mkRequestLogger $
           def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }
       let settings = setPort (port conf) defaultSettings
       let application = logger $ serve api $ server svc
       waitForStop (runSettings settings application)
                   (destroyAllResources $ R.unRepo repo)
