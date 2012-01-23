{-# language PatternSignatures #-}
{-# language DeriveDataTypeable #-}

module Game where

import Prelude hiding ( catch )

import Spieler
import Wurf
import State
import Bank
import Registrar
import Call

import qualified Data.Map as M
import Data.Typeable

import System.IO
import Control.Monad ( when, void, forM, forM_ )
import System.Random

import Control.Concurrent
import Control.Concurrent.STM

import Control.Exception
import qualified System.Timeout

import Network.XmlRpc.Client

import Data.Acid ( update )


-- | choose a subset of players (with at least two)
-- have them play a game, record the result
game :: Server -> IO ()
game server = void $ do

    xs <- select_players server

    Control.Exception.catch ( do 
                                 
        message server $ Game xs
        verify_callbacks server xs
        
        winner <- play_game server xs 
        message server $ Game_Won_By winner
        
        update ( bank server ) $ Updates
            $ (name winner, 0, 1 ) 
            : zip3 (map name xs) (repeat 1) (repeat 0) 
            
      ) $ \ ( e :: SomeException ) -> do
        os <- atomically $ do 
            os <- readTVar $ offenders server
            writeTVar ( offenders server ) []
            r <- readTVar $ registry server
            writeTVar ( registry server ) 
                $ M.difference r
                $ M.fromList $ zip (map name os) 
                             $ repeat ()
            return os    
        update ( bank server ) $ Updates
            $ zip3 (map name os) 
                   (repeat 0) (repeat $ negate 1 )
        message server $ Protocol_Error_By os

select_players server = do
    xs <- atomically $ do
        m <- readTVar $ registry server
        check $ M.size m >= 2
        return $ M.elems m
    ys <- permute xs    
    n <- randomRIO ( 2, length ys )
    return $ take n ys

permute :: [a] -> IO [a]
permute [] = return []
permute xs = do
    k <- randomRIO ( 0, length xs - 1 )
    let (pre, this : post ) = splitAt k xs
    rest <- permute $ pre ++ post
    return $ this : rest
            
------------------------------------------------------

verify_callbacks :: Server -> [ Spieler ] -> IO ()
verify_callbacks server xs = forM_ xs $ \ x ->  do
    res <- logged0 server x "Player.who_are_you"
    when ( name x /= res ) $ do
        message server $ Callback_Mismatch x res
        add_offender server x
        throwIO $ ProtocolE x

-- | Resultat: der Gewinner (alle anderen sind raus)
play_game :: Server -> [ Spieler ] -> IO Spieler
play_game server ys = bracket_
    ( forM ys $ \ y -> ignore_errors server ( logged0 server y "Player.begin_game" :: IO Bool ) )
    ( forM ys $ \ y -> ignore_errors server ( logged0 server y "Player.end_game"   :: IO Bool ) ) $ do
        continue_game server ys

continue_game server ys = case ys of
    [] -> error "play_game []"
    [winner] -> return winner
    _ -> do 
        ( loser, rest )  <- play_round server ys
        continue_game server rest
        
-- | Resultat: der Verlierer und der Rest (der weiterspielen darf)
play_round :: Server 
           -> [ Spieler ] ->  IO (Spieler, [Spieler])
play_round server (s : ss) = bracket_
    ( forM (s:ss) $ \ y -> ignore_errors server ( logged0 server y "Player.begin_round" :: IO Bool ) )
    ( forM (s:ss) $ \ y -> ignore_errors server ( logged0 server y "Player.end_round"   :: IO Bool ) ) $ do
        message server $ Round (s:ss)
        w <- roll
        w' <- logged1 server s "Player.say" w
        (loser, rest) <- continue_round server (ss ++ [s]) (w, w')
        message server $ Round_Lost_By loser
        return (loser, rest)
    
continue_round server (s : ss) (echt, ansage) = do    
    message server $ Bid ansage  
    threadDelay $ 10^6
  
    forM ss $ \ s' -> ( logged1 server s' "Player.other" ansage :: IO Bool )
    a <- logged1 server s "Player.accept" ansage
    if a 
       then do -- weiterspielen
         echt' <- Wurf.roll
         ansage' <- logged1 server s "Player.say" echt'
         if ansage' <= ansage 
             then return (s, ss) -- verloren
             else continue_round server (ss ++ [s]) ( echt', ansage' )     
       else do -- aufdecken
         if echt >= ansage 
            then return ( s, ss )
            else return ( last ss , s : init ss )

--------------------------------------------------------------------

            



