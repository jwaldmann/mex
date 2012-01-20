{-# language DoAndIfThenElse #-}

module Registrar where

import Spieler
import State

import qualified Network.Wai.Frontend.MonadCGI

import Network.CGI
import Network.XmlRpc.Server
import Control.Monad ( when )
import Control.Concurrent.STM
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
    ok <- atomically $ do 
              m <- readTVar $ registry state
              let not_logged_in = not $ M.member ( name s ) m
                  unique_callback = 
                       M.null $ M.filter ( \ t -> callback s == callback t ) m
                  ok = not_logged_in && unique_callback 
              if ok 
              then do   
                      writeTVar ( registry state ) $ M.insert ( name s ) s m
                      return True
              else return False
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

    


