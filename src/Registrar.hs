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
    [ ("Server.login", fun $ login passwd_map state ) ]
      
login :: M.Map Name Password -> Server -> Spieler -> IO Bool
login passwd_map state s = do  
    t <- getCurrentTime          
    hPutStrLn stderr $ unwords [ "login", show t, show s ]
    let password_ok = Just ( password s ) == M.lookup ( name s ) passwd_map
    let reg = registry state    
    ok <- if not password_ok then return False else  atomically $ do 
              m <- readTVar reg
              if M.null $ M.filter ( \ t -> callback s == callback t ) m
                  then do   
                      writeTVar reg $ M.insert ( name s ) s m
                      return True
                  else return False
    hPutStrLn stderr $ unwords [ "login", show t, show s, show ok ]
    return ok

    


