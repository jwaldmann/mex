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

data State = State { name :: Name, port :: Int
                   , previous ::  TVar ( Maybe Wurf )  
                   }  
             
fresh :: Name -> Int -> IO State
fresh n p = do
    prev <- atomically $ newTVar Nothing  
    return $ State { Main.name = n, port = p 
                   , previous = prev }

-- | command line arguments: 
-- name, password, callback URL, server URL.
-- the port number for this player's server
-- is extracted from the callback URL
main  = do
    [ n, p, client , server ] <- getArgs
    forkIO $ do
        threadDelay $ 10^6
        True <- remote server "Server.login" $ Spieler 
              { Spieler.name = Name n
              , password = Password p
              , callback = Callback client
              } 
        return ()
    let extract_port = reverse . takeWhile (/= ':') . reverse    
    state <- fresh ( Name n ) 
                 ( read $ extract_port client )     
    play state
    
play state
     = Network.Wai.Handler.Warp.runSettings 
       ( defaultSettings { settingsTimeout = 1
                         , settingsPort = port state} ) 
     $ Network.Wai.Frontend.MonadCGI.cgiToApp
     $ do
         input <- getBody
         result <- liftIO 
                   $ handleCall ( server state ) input
         outputFPS result

server state = methods 
    [ ("Player.who_are_you", fun $ who_are_you state )
    , ("Player.begin_round", fun $ begin state )
    , ("Player.end_round", fun $ end state )
    , ("Player.begin_game", fun $ begin state )
    , ("Player.end_game", fun $ end state )
    , ("Player.accept", fun $ accept state )
    , ("Player.other", fun $ ignore state )
    , ("Player.say", fun $ say state ) 
    ]
    
who_are_you :: State -> IO Name
who_are_you s = return $ Main.name s

begin :: State -> IO ()
begin s = do
   atomically $ writeTVar ( previous s ) Nothing

end :: State -> IO ()
end s = do
   atomically $ writeTVar ( previous s ) Nothing

ignore :: State -> Wurf -> IO ()
ignore s w = return ()

accept :: State -> Wurf -> IO Bool
accept s w = do
   atomically $ writeTVar ( previous s ) $ Just w
   return $ w < wurf 6 3
   
say :: State -> Wurf -> IO Wurf
say s w = do
   prev <- atomically $ readTVar ( previous s )
   return $ case prev of
       Just u | u >= w -> succ u
       _ -> w

