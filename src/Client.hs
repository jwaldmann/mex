{-# language PatternSignatures #-}

module Main where

import Wurf
import Spieler
import Bank

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
        
    score <- remote server "Server.scores" :: IO Bank
    print score
        
    let extract_port = reverse . takeWhile (/= ':') . reverse    
    state <- fresh ( Name n ) 
                 ( read $ extract_port client )     
    
    when False $ play state
    
    
    
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
    , ("Player.end_round", fun $ ignore0 state )
    , ("Player.begin_game", fun $ ignore0 state )
    , ("Player.end_game", fun $ ignore0 state )
    , ("Player.accept", fun $ accept state )
    , ("Player.other", fun $ ignore1 state )
    , ("Player.say", fun $ say state ) 
    , ("Player.game_won_by", fun (( \ s -> return True ) :: Name -> IO Bool ))
    , ("Player.round_lost_by", fun (( \ s -> return True ) :: Name -> IO Bool ))
    ]
    
who_are_you :: State -> IO Name
who_are_you s = return $ Main.name s

begin :: State -> IO Bool
begin s = do
   atomically $ writeTVar ( previous s ) Nothing
   return True

ignore0 :: State -> IO Bool
ignore0 s = return True

ignore1 :: State -> Wurf -> IO Bool
ignore1 s w = return True

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

