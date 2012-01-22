module State where

import Spieler
import Wurf
import Bank

import Control.Concurrent.STM
import Data.Time
import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ

data Message = Login Spieler Bool 
             | Callback_Mismatch Spieler Name
             | Logout Spieler Bool  
             | Game [ Spieler ] 
             | Round [ Spieler ]
             | Bid Wurf  
             | Game_Won_By Spieler
             | Round_Lost_By Spieler  
             | Protocol_Error_By [ Spieler ]
             | RPC_Call Spieler String
             | RPC_Return_OK
             | RPC_Error String
    deriving Show               

type Registry = M.Map Name Spieler

data Server = Server { registry  :: TVar Registry
                 , bank      :: TVar Bank
                 , messages :: TVar [ ( UTCTime,  Message ) ] 
                 , offenders :: TVar [ Spieler ]  
                 }

pretty :: [ (UTCTime, Message) ] -> Doc
pretty ums = text "most recent actions:" $$ nest 4 ( vcat $ do
    (u, m) <- ums
    let sized n s = s ++ replicate ( n - length s ) ' ' 
    return $ text ( sized 60 $ show m ) <+> text ( show u )
  )  

message :: Server -> Message -> IO ()
message s m = do
    t <- getCurrentTime
    atomically $ do
        let p = messages s
        ms <- readTVar p    
        writeTVar p $ take 50 $ (t, m) : ms 
    print ( t, m )    

make = do 
    re <- atomically $ newTVar M.empty
    ba <- atomically $ newTVar M.empty
    os <- atomically $ newTVar []
    ms <- atomically $ newTVar []
    return $ Server { registry = re, bank = ba 
                        , offenders = os , messages = ms
                        }
    
