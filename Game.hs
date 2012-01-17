{-# language PatternSignatures #-}
{-# language DeriveDataTypeable #-}

module Game where

import Spieler
import Wurf
import Bank
import Registrar
import Void

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

data ProtocolE = ProtocolE Spieler deriving ( Show, Typeable )
instance Exception ProtocolE

data TimeoutE = TimeoutE  deriving ( Show, Typeable )
instance Exception TimeoutE

data Server = Server { registry  :: TVar Registry
                 , bank      :: TVar Bank
                 }

second = 10^ 6
timeout = 10 * second

timed :: Int -> IO a -> IO a
timed to action = do
    res <- System.Timeout.timeout to action 
    case res of
        Nothing -> throwIO TimeoutE
        Just res -> return res

remove :: Server -> Spieler -> IO ()
remove server s = atomically $ do 
    m <- readTVar $ registry server
    writeTVar ( registry server ) $ M.delete ( name s ) m

-- | choose a subset of players (with at least two)
-- have them play a game, record the result
game :: Server -> IO ()
game server = void $ do

    xs <- select_players server

    result <- Control.Exception.catch ( fmap Right $ play_game server xs ) 
        $ \ ( ProtocolE s ) -> return $ Left s

    atomically $ do            
        m <- readTVar $ bank server
        writeTVar ( bank server ) $ Bank.update ( map name xs ) 
                       ( case result of 
                            Right p -> ( name p ,  1 )
                            Left  p -> ( name p , -1 )
                       ) m

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
            
----------------------------------------------------------------------

-- | Resultat: der Gewinner (alle anderen sind raus)
play_game :: Server -> [ Spieler ] -> IO Spieler
play_game server ys = bracket_
    ( forM ys $ \ y -> logged0 y "begin_game" :: IO () )
    ( forM ys $ \ y -> logged0 y "end_game"   :: IO () ) $ do
        continue_game ys

continue_game ys = case ys of
    [] -> error "play_game []"
    [winner] -> return winner
    _ -> do 
        ( loser, rest )  <- play_round ys
        continue_game rest
        
-- | Resultat: der Verlierer und der Rest (der weiterspielen darf)
play_round :: [ Spieler ] ->  IO (Spieler, [Spieler])
play_round (s : ss) = bracket_
    ( forM (s:ss) $ \ y -> logged0 y "begin_round" :: IO () )
    ( forM (s:ss) $ \ y -> logged0 y "end_round"   :: IO () ) $ do
        w <- roll
        w' <- logged1 s "say" w
        (loser, rest) <- continue_round (ss ++ [s]) (w, w')
        return (loser, rest)
    
continue_round (s : ss) (echt, ansage) = do    
    forM ss $ \ s' -> logged1 s' "other" ansage :: IO ()
    a <- logged1 s "accept" ansage
    if a 
       then do -- weiterspielen
         echt' <- Wurf.roll
         ansage' <- logged1 s "say" echt'
         if ansage' <= ansage 
             then return (s, ss) -- verloren
             else continue_round (ss ++ [s]) ( echt', ansage' )     
       else do -- aufdecken
         if echt >= ansage 
            then return ( s, ss )
            else return ( last ss , s : init ss )

--------------------------------------------------------------------

logging = True

logged0 s cmd = do
    let Callback c = callback s
    res <- handle ( \ ( e :: SomeException ) -> throwIO $ ProtocolE s ) 
         $ timed timeout
         $ remote c cmd 
    when logging $ hPutStrLn stderr 
         $ unwords [ cmd, show $ name s, "=>", show res ]
    return res

logged1 s cmd arg = do
    let Callback c = callback s
    res <- handle ( \ ( e :: SomeException ) -> throwIO $ ProtocolE s ) 
         $ timed timeout $ remote c cmd arg
    when logging $ hPutStrLn stderr 
         $ unwords [ cmd, show $ name s, show arg, "=>", show res ]
    return res




