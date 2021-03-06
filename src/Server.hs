{-# language OverloadedStrings #-}

module Main where

import Spieler
import Game
import Bank
import Registrar
import Logger
import State
import Chart ( chart_location )
import Rating ( taxman, taxman2, chartman )

import Network.Wai.Handler.Warp
import Network.HTTP.Types (statusOK)
import Network.Wai

import Control.Concurrent
import Control.Concurrent.STM


import System.IO
import Control.Monad ( forever )

import qualified Data.Map as M
import System.Environment

import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class ( liftIO )

main = do
    hSetBuffering stderr LineBuffering
    hSetBuffering stdout LineBuffering
    
    [ port, passwd_file ] <- getArgs
    passwd <- readFile passwd_file
    let passwd_map = M.fromList $ do 
            l <- lines passwd ; [ n, p ] <- return $ words l 
            return ( Name n, Password p )
    
    server <- State.make passwd_map
    forkIO $ forever $ game server

    forkIO $ forever $ do 
        chartman server   
        threadDelay ( 60 * 10 ^ 6 )
        taxman2 server

    Network.Wai.Handler.Warp.runSettings 
      ( defaultSettings { settingsTimeout = 1
                        , settingsPort = read port } 
      ) $ \ req -> case pathInfo req of
      [ "rpc" ] -> registrar server req
      [ "log" ] -> logger server req
      [ "chart" ] -> do
            s <- liftIO $ LBS.readFile chart_location
            return 
               $ responseLBS statusOK [("Content-Type", "image/png")] s 
      _         -> return 
            $ responseLBS statusOK [("Content-Type", "text/plain")] 
            $ "the server is here, but the service url is wrong"







