module State where

import Spieler
import Wurf
import Bank
import Chart

import Data.Acid
import Control.Concurrent.STM
import Control.Monad ( guard )
import Data.Time
import qualified Data.Map as M
import qualified Data.Set as S
import Text.PrettyPrint.HughesPJ

message_queue_length = 1000

protocol_messages_display_length = 50
game_messages_display_length = 50

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
             | Rating_Snapshot
             | Taxman
    deriving Show               

is_game_message m = case m of
    Game _ -> True
    Round _ -> True
    Bid _ -> True
    Game_Won_By _ -> True
    Round_Lost_By _ -> True
    _ -> False

type Registry = M.Map Name Spieler

data Server = Server { registry  :: TVar Registry
                 , bank_lock :: TVar Bool
                 , bank      :: AcidState Bank
                 , messages :: TVar [ ( UTCTime,  Message ) ] 
                 , offenders :: TVar ( S.Set Spieler )
                 , chart :: AcidState Chart
                 }

pretty :: [ (UTCTime, Message) ] -> Doc
pretty ums = contents ums $$ protocol ums

contents ums = 
  text "most recent (game) actions:" $$ nest 4 ( vcat 
    $ take game_messages_display_length $ do
        (u, m) <- ums
        guard $ is_game_message m
        let sized n s = s ++ replicate ( n - length s ) ' ' 
        return $ text ( sized 60 $ show m ) <+> text ( show u )
  )  
    
protocol ums = 
  text "most recent (protocol) actions:" $$ nest 4 ( vcat 
    $ take protocol_messages_display_length $ do
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
        writeTVar p $ take message_queue_length $ (t, m) : ms 
    print ( t, m )    

make = do 
  
    bank_state <- openLocalState $ Bank.empty
    createCheckpoint bank_state

    chart_state <- openLocalState $ Chart.empty
    createCheckpoint chart_state

    re <- atomically $ newTVar M.empty
    os <- atomically $ newTVar S.empty
    ms <- atomically $ newTVar []
    lo <- atomically $ newTVar True
    
    return $ Server { registry = re, bank = bank_state, bank_lock = lo
                    , offenders = os , messages = ms
                    , chart = chart_state
                    }
    
    
