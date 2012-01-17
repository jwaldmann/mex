{-# language OverloadedStrings #-}

module Logger where

import Bank 
import Registrar

import qualified Network.Wai.Handler.Warp
import Network.HTTP.Types (statusOK)
import Network.Wai (responseLBS)
import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString.Lazy.Char8 as C
import Data.DateTime
import Text.PrettyPrint.HughesPJ
import Control.Concurrent.STM

logger bank registry = \ req -> do
     b <- liftIO $ atomically $ readTVar bank
     r <- liftIO $ atomically $ readTVar registry
     t <- liftIO $ getCurrentTime
     let dash = text $ replicate 50 '-'
     return $ responseLBS statusOK [("Content-Type", "text/plain")] 
            $ C.pack $ show $ vcat 
            [ text $ show t , dash  
            , Bank.pretty b, dash 
            , Registrar.pretty r , dash
            , text "built with: warp, wai, conduit; see http://www.yesodweb.com/"
            ]


