{-# language OverloadedStrings #-}

module Logger where

import Bank 
import Registrar
import State

import qualified Network.Wai.Handler.Warp
import Network.HTTP.Types (statusOK)
import Network.Wai (responseLBS)
import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString.Lazy.Char8 as C
import Data.DateTime
import Text.PrettyPrint.HughesPJ
import Control.Concurrent.STM

logger state = \ req -> do
     t <- liftIO $ getCurrentTime
     b <- liftIO $ atomically $ readTVar $ bank state
     r <- liftIO $ atomically $ readTVar $ registry state
     ms <- liftIO $ atomically $ readTVar $ messages state
     let dash = text $ replicate 50 '-'
     return $ responseLBS statusOK [("Content-Type", "text/plain")] 
            $ C.pack $ show $ vcat 
            [ text $ show t , dash  
            , Bank.pretty b, dash 
            , Registrar.pretty r , dash
            , State.pretty ms , dash                       
            , text "built with: warp, wai, conduit; see http://www.yesodweb.com/"
            ]


