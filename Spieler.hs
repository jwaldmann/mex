{-# language PatternSignatures #-}
{-# language GeneralizedNewtypeDeriving #-}

module Spieler where

import Network.XmlRpc.Internals
import Control.Monad.Error

newtype Name = Name String deriving ( Eq, Ord, Show, Read, XmlRpcType )
newtype Password = Password String deriving ( Eq, Ord, Show, Read, XmlRpcType )
newtype Callback = Callback String deriving ( Eq, Ord, Show, XmlRpcType )

data Spieler = Spieler 
               { name :: Name
               , password :: Password
               , callback :: Callback
               }  
   deriving ( Ord, Eq, Show )
            
instance XmlRpcType Spieler where
  getType s = TStruct
  toValue s = ValueStruct 
     [ ("name", toValue $ name s )
     , ("password", toValue $ password s )  
     , ("callback", toValue $ callback s )  
     ]  
  fromValue v = case v of   
     ValueStruct vs -> do
         n <- getField "name" vs
         p <- getField "password" vs
         c <- getField "callback" vs
         return $ Spieler { name = n, password = p, callback = c }
     _ -> ErrorT $ return $ Left "Spieler" 

