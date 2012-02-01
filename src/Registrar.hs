{-# language DoAndIfThenElse #-}

module Registrar where

import Spieler
import State
import Bank 
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
import Data.Acid ( query )

pretty :: Registry -> Doc
pretty reg = text "currently logged in:" 
   <+> fsep ( do ( Name k, v ) <- M.toList reg ; return $ text k )
       

registrar state = 
    Network.Wai.Frontend.MonadCGI.cgiToApp $ do
         input <- getBody
         result <- liftIO $ handleCall ( server state ) input
         outputFPS result

server state = 
  let passwd_map = passwords state 
  in  methods 
    [ ("Server.login", fun $ password_check passwd_map $ login state )
    , ("Server.logout", fun $ password_check passwd_map $ logout state ) 
    , ("Server.scores", fun $ scores state )
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
        let ok = not $ M.member ( name s ) m
        when ok $ do
            writeTVar ( registry state ) 
                  $ M.insert ( name s ) s m
        return ok      
    message state $ Login s ok
    return ok

logout :: Server -> Spieler -> IO Bool
logout state s = do  
    ok <- atomically $ do 
              m <- readTVar $ registry state
              let ok =  M.member ( name s ) m
              when ok $ do   
                      writeTVar ( registry state ) $ M.delete ( name s ) m
              return ok
    message state $ Logout s ok
    return ok

scores :: Server -> IO Bank
scores state = query ( bank state ) Snapshot
    


