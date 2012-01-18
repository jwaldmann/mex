{-# language PatternSignatures #-}

module Main where

import Wurf
import Spieler
import Void

import Network.Wai.Handler.Warp
import qualified Network.Wai.Frontend.MonadCGI

import Network.HTTP.Types (statusOK)
import Network.Wai (responseLBS)
import Network.CGI

import Network.XmlRpc.Server
import Network.XmlRpc.Client

import System.Environment
import System.IO

import Control.Monad ( when )
import Control.Concurrent.STM
import Control.Concurrent

main  = do
    [ n, p, port , server ] <- getArgs
    prev <- atomically $ newTVar Nothing
    forkIO $ do
        threadDelay $ 10^6
        True <- remote server "login" $ Spieler 
              { name = Name n
              , password = Password p
              , callback = Callback $ "http://localhost:" ++ port
              } 
        return ()
    play ( read port ) prev
    
play port prev 
     = Network.Wai.Handler.Warp.runSettings 
       ( defaultSettings { settingsTimeout = 1, settingsPort = port } ) 
     $ Network.Wai.Frontend.MonadCGI.cgiToApp
     $ do
         input <- getBody
         result <- liftIO $ handleCall ( server prev ) input
         outputFPS result

server prev = methods 
    [ ("Player.begin_round", fun $ begin prev )
    , ("Player.end_round", fun $ end prev )
    , ("Player.begin_game", fun $ begin prev )
    , ("Player.end_game", fun $ end prev )
    , ("Player.accept", fun $ accept prev )
    , ("Player.other", fun $ ignore prev )
    , ("Player.say", fun $ say prev ) 
    ]

begin :: TVar ( Maybe Wurf ) -> IO ()
begin p = do
   atomically $ writeTVar p Nothing

end :: TVar ( Maybe Wurf ) -> IO ()
end p = do
   atomically $ writeTVar p Nothing

ignore :: TVar (Maybe Wurf) -> Wurf -> IO ()
ignore p w = return ()

accept :: TVar (Maybe Wurf) -> Wurf -> IO Bool
accept p w = do
   atomically $ writeTVar p $ Just w
   return $ w < wurf 6 3
   
say :: TVar (Maybe Wurf) -> Wurf -> IO Wurf
say p w = do
   prev <- atomically $ readTVar p
   return $ case prev of
       Just u | u >= w -> succ u
       _ -> w


