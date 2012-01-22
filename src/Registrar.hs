{-# language DoAndIfThenElse #-}

module Registrar where

import Spieler
import State
import Call

import qualified Network.Wai.Frontend.MonadCGI

import Network.CGI
import Network.XmlRpc.Server
import Network.XmlRpc.Client
import Control.Monad ( when )
import Control.Concurrent.STM
import Control.Exception
import System.IO
import qualified Data.Map as M
import Data.Time
import Text.PrettyPrint.HughesPJ

pretty :: Registry -> Doc
pretty reg = text "currently logged in:" <+> vcat (
    do ( k, v ) <- M.toList reg
       return $ text ( show k ) <+> text ( show $ callback v ) )

registrar passwd_map state = 
    Network.Wai.Frontend.MonadCGI.cgiToApp $ do
         input <- getBody
         result <- liftIO $ handleCall ( server passwd_map state ) input
         outputFPS result

server passwd_map state = methods 
    [ ("Server.login", fun $ password_check passwd_map $ login state )
    , ("Server.logout", fun $ password_check passwd_map $ logout state ) 
    ]
    
password_check :: M.Map Name Password
               -> ( Spieler -> IO Bool ) 
               -> ( Spieler -> IO Bool )
password_check passwd_map action s = do    
    let password_ok = Just ( password s ) == M.lookup ( name s ) passwd_map
    if password_ok then action s else return False
    
login :: Server -> Spieler -> IO Bool
login state s = do  
    not_logged_in <- atomically $ do 
              m <- readTVar $ registry state
              return $ not $ M.member ( name s ) m
    callback_match <- Control.Exception.handle 
       ( \ ( ProtocolE s ) -> return False ) $ do
           res <- logged0 state s "Player.who_are_you"
           if name s == res
           then return True
           else do
               message state $ Callback_Mismatch s res
               return False
    let ok = not_logged_in && callback_match
    when ok $ atomically $ do   
        m <- readTVar $ registry state
        writeTVar ( registry state ) 
                  $ M.insert ( name s ) s m
    message state $ Login s ok
    return ok

logout :: Server -> Spieler -> IO Bool
logout state s = do  
    ok <- atomically $ do 
              m <- readTVar $ registry state
              let logged_in = M.member ( name s ) m
              if  logged_in
              then do   
                      writeTVar ( registry state ) $ M.delete ( name s ) m
                      return True
              else return False
    message state $ Logout s ok
    return ok

    


