module State where

import Spieler
import Bank

import Control.Concurrent.STM
import Data.Time
import qualified Data.Map as M

data Message = Begin_Game [ Spieler ] 
             | Begin_Round [ Spieler ]
             | End_Game 
             | End_Round
             | Game_Won_By Spieler
             | Round_Lost_By Spieler  
             | Protocol_Error_By Spieler  
             | RPC_Call Spieler  
             | RPC_Return_OK
             | RPC_Error   
    deriving Show               

type Registry = M.Map Name Spieler

data Server = Server { registry  :: TVar Registry
                 , bank      :: TVar Bank
                 , messages :: TVar [ ( UTCTime,  Message ) ] 
                 , offenders :: TVar [ Spieler ]  
                 }

make = do 
    re <- atomically $ newTVar M.empty
    ba <- atomically $ newTVar M.empty
    os <- atomically $ newTVar []
    ms <- atomically $ newTVar []
    return $ Server { registry = re, bank = ba 
                        , offenders = os , messages = ms
                        }
    
