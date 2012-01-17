{-# language OverloadedStrings #-}

module Main where

import Spieler
import Game
import Bank
import Registrar
import Logger


import Network.Wai.Handler.Warp
import Network.HTTP.Types (statusOK)
import Network.Wai

import Control.Concurrent
import Control.Concurrent.STM


import System.IO
import Control.Monad ( forever )

import qualified Data.Map as M
import System.Environment

main = do
    hSetBuffering stderr LineBuffering
    hSetBuffering stdout LineBuffering
    
    [ port, passwd_file ] <- getArgs
    passwd <- readFile passwd_file
    let passwd_map = M.fromList $ do 
            l <- lines passwd ; [ n, p ] <- return $ words l 
            return ( Name n, Password p )
    
    re <- atomically $ newTVar M.empty
    ba <- atomically $ newTVar M.empty
    let server = Server { registry = re, bank = ba }
    
    forkIO $ forever $ game server

    Network.Wai.Handler.Warp.runSettings 
      ( defaultSettings { settingsTimeout = 1
                        , settingsPort = read port } 
      ) $ \ req -> case pathInfo req of
      [ "rpc" ] -> registrar passwd_map ( registry server ) req
      [ "log" ] -> logger (bank server) ( registry server) req
      _         -> return 
            $ responseLBS statusOK [("Content-Type", "text/plain")] 
            $ "the server is here, but the service url is wrong"







