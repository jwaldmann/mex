module Rating where

import Spieler
import Bank
import State

import Data.Acid
import Control.Concurrent.STM

import qualified Data.Map as M
import qualified Data.Set as S

elo_base       = 1000.0
elo_max_adjust =   10.0

process_regular_game_result 
    :: Server -> [ Spieler ] -> Spieler -> IO ()
process_regular_game_result state players winner = do
    Bank b <- query ( bank state ) Snapshot
    
    let weights = do 
            p <- players
            let k = M.findWithDefault blank (name p) b
            return ( name p, exp ( rating k / elo_base ))
        total = sum $ map snd weights
        drift = M.fromList $ do
            (p, w) <- weights
            let expected = w / total
                actual = 
                  if p == name winner then 1 else 0
            return (p, elo_max_adjust * ( actual - expected ))
    
    update ( bank state ) $ Updates $ M.fromList $ do
        s <- players
        let p = if s == winner 
                then length players - 1 else 0
        let k = M.findWithDefault blank (name s) b
        return ( name s, k { played = 1 + played k
                           , points = p + points k 
                           , rating = drift M.! name s + rating k          
                           } )           
        
-- | log them out, and adjust accounts
process_offenses :: Server -> IO ()
process_offenses server = do
    os <- atomically $ do
          os <- readTVar $ offenders server
          writeTVar ( offenders server ) S.empty
          r <- readTVar $ registry server
          writeTVar ( registry server ) 
                $ M.difference r
                $ M.fromList 
                $ zip ( map name $ S.toList os ) 
                $ repeat ()
          return os
    message server $ Protocol_Error_By $ S.toList os
    Bank b <- query ( bank server ) Snapshot
    update ( bank server ) $ Updates $ M.fromList $ do
          o <- S.toList os
          let k = M.findWithDefault blank (name o) b
          return ( name o
                 , k { protocol_errors = protocol_errors k + 1 } ) 

blank ::Konto
blank = Konto { played = 0, points = 0
             , protocol_errors = 0
             , rating = elo_base
             }           
