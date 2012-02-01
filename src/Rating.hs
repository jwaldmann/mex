module Rating where

import Spieler
import Bank
import State
import Chart

import Data.Acid
import Control.Concurrent.STM
import Control.Monad ( when )

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Either
import Data.Time

elo_base       = 1000.0
elo_max_adjust =   10.0

taxation = 0.5 / elo_base

modify :: Server -> ( Bank -> M.Map Name Konto ) -> IO ()
modify s f = do
    atomically $ do 
        l <- readTVar ( bank_lock s ) 
        check l
        writeTVar ( bank_lock s ) False
    b <- query ( bank s ) Bank.Snapshot
    update ( bank s ) $ Updates $ f b
    atomically $ do writeTVar ( bank_lock s ) True

-- | linear tax for all players that are not logged in.
-- the total tax will be distributed evenly among all players
-- that are logged in.
taxman2 :: Server -> IO ()
taxman2 state = do
    message state Taxman
    logged_in <- atomically $ readTVar $ registry state
    when ( not $ M.null logged_in ) 
         $ modify state $ \ ( Bank b ) -> M.fromList $ do
            let balance = do
                    (p, k) <- M.toList b -- all known players
                    return ( p ,
                            if M.member p logged_in
                            then Right $ total / fromIntegral ( M.size logged_in )
                            else Left $ rating k * taxation
                           )      
                total = sum $ lefts $ map snd $ balance
            ( p, bal ) <- balance
            let k = b M.! p
            return ( p, k { rating = rating k + either negate id bal } )

-- | tax is 1/i for player of rank i.
-- the total tax will be distributed evenly among all players 
taxman :: Server -> IO ()
taxman state = do
    message state $ Taxman 
    modify state $ \ ( Bank b ) -> M.fromList $ do
        let taxes = do
                (i, (p, k)) <- zip [ 1 .. ] 
                       $ sortBy ( comparing ( negate . rating . snd ))
                       $ M.toList b
                return ( p, 1.0 / fromIntegral i )
            total = sum $ map snd taxes
            support = total / fromIntegral ( M.size b )
        ( p, tax ) <- taxes
        let k = b M.! p
        return ( p, k { rating = rating k + support - tax } )


process_regular_game_result 
    :: Server -> [ Spieler ] -> Spieler -> IO ()
process_regular_game_result state players winner =
  modify state $ \ ( Bank b )-> M.fromList $ do
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
    s <- players
    let p = if s == winner 
                then length players - 1 else 0
    let k = M.findWithDefault blank (name s) b
    return ( name s, k { played = 1 + played k
                           , points = p + points k 
                           , rating = drift M.! name s + rating k          
                           } )       

chartman state = do
    message state Rating_Snapshot
    now <- getCurrentTime 
    b <- query ( bank state ) Bank.Snapshot
    update ( chart state ) $ Add now b
    ch <- query ( chart state ) Chart.Snapshot
    Chart.write ch
        
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
    Bank b <- query ( bank server ) Bank.Snapshot
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
