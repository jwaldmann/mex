{-# language PatternSignatures #-}

import Network.XmlRpc.Client

import Control.Monad 
import System.Random

main = forever $ do
    x :: Int <- randomRIO ( 0, 1000 )  
    y :: Int <- randomRIO ( 0, 1000 )  
    z :: Int <- remote "http://localhost:9999" "leak" x y
    print (x,y,z)
