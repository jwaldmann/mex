{-# language PatternSignatures #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell #-}

module Spieler where

import Network.XmlRpc.Internals
import Control.Monad.Error

import Data.SafeCopy

newtype Name = Name String deriving ( Eq, Ord, Show, Read, XmlRpcType )
newtype Password = Password String deriving ( Eq, Ord, Show, Read, XmlRpcType )
newtype Callback = Callback String deriving ( Eq, Ord, Show, XmlRpcType )

$(deriveSafeCopy 0 'base ''Name)
$(deriveSafeCopy 0 'base ''Password)
$(deriveSafeCopy 0 'base ''Callback)

data Spieler = Spieler 
               { name :: Name
               , password :: Password
               , callback :: Callback
               }  
   deriving ( Ord, Eq )
            
instance Show Spieler where
   show s = let Name n = name s in n
            
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

