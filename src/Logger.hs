{-# language OverloadedStrings #-}

module Logger where

import Bank 
import Registrar
import State

import Prelude hiding (head, id, div)
import Text.Blaze.Html4.Strict hiding (map)
import Text.Blaze.Html4.Strict.Attributes hiding (title)
import Text.Blaze.Renderer.Utf8 (renderHtml)

import qualified Network.Wai.Handler.Warp
import Network.HTTP.Types (statusOK)
import Network.Wai (responseLBS)
import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString.Lazy.Char8 as C
import Data.DateTime
-- import Text.PrettyPrint.HughesPJ
import Control.Concurrent.STM
import Data.Acid ( query )

logger state = \ req -> do
     t <- liftIO $ getCurrentTime
     b <- liftIO $ query ( bank state ) Snapshot
     r <- liftIO $ atomically $ readTVar $ registry state
     ms <- liftIO $ atomically $ readTVar $ messages state

     return $ responseLBS statusOK [("Content-Type", "text/html")] 
       $ renderHtml $ html $ do
         let ti = "Projekt Softwaretechnik I WS 11/12"
         head $ do title ti
         body $ do
           h3 ti
           p $ toHtml $ show t 
           p $ pre $ toHtml $ show $ Bank.pretty b
           p $ img ! src "/chart"
           p $ references
           p $ pre $ toHtml $ show $ Registrar.pretty r
           p $ pre $ toHtml $ show $ State.pretty ms
           
references = do
    a "source" ! href "https://github.com/jwaldmann/mex"
    " | libraries: "
    a "web server: warp/wai" ! href "http://www.yesodweb.com/" 
    ","
    a "persistence: acid-state" ! href "http://acid-state.seize.it/" 
    ","
    a "xml rpc server/client: haxr" ! href  "http://hackage.haskell.org/package/haxr"
    ","
    a "graph: Chart" ! href "http://hackage.haskell.org/package/Chart"
    ","
    a "text: blaze-html" ! href "http://hackage.haskell.org/package/blaze-html"


