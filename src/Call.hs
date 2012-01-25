{-# language PatternSignatures #-}
{-# language DeriveDataTypeable #-}

module Call where

import Spieler
import State

import Control.Concurrent.STM
import Control.Exception
import qualified System.Timeout
import Network.XmlRpc.Client
import Control.Monad ( void )
import Data.Typeable
import qualified Data.Set as S

data ProtocolE = ProtocolE Spieler deriving ( Show, Typeable )
instance Exception ProtocolE

data TimeoutE = TimeoutE  deriving ( Show, Typeable )
instance Exception TimeoutE


second = 10^ 6
timeout = 10 * second

timed :: Int -> IO a -> IO a
timed to action = do
    res <- System.Timeout.timeout to action 
    case res of
        Nothing -> throwIO TimeoutE
        Just res -> return res


logging = False

logged0 server s cmd = do
    let Callback c = callback s
    message server $ RPC_Call s cmd    
    handle ( \ ( e :: SomeException ) -> do
                message server $ RPC_Error $ show e
                add_offender server s
                throwIO $ ProtocolE s ) 
         $ timed timeout
         $ remote c cmd 

logged1 server s cmd arg = do
    message server $ RPC_Call s cmd    
    let Callback c = callback s
    handle ( \ ( e :: SomeException ) -> do
                message server $ RPC_Error $ show e
                add_offender server s
                throwIO $ ProtocolE s ) 
         $ timed timeout 
         $ remote c cmd arg

add_offender server s = atomically $ do
    os <- readTVar $ offenders server
    writeTVar ( offenders server ) $ S.insert s os

ignore_errors server action = 
    handle ( \ ( SomeException e ) -> return () ) ( void action ) 


