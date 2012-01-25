{-# language DeriveDataTypeable, TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}

module Bank where

import Spieler

import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ
import Data.List ( sortBy )
import Data.Ord ( comparing )

import Data.SafeCopy
import Data.Typeable
import Data.Acid
import Control.Monad.State
import Control.Monad.Reader

import Network.XmlRpc.THDeriveXmlRpcType
import Network.XmlRpc.Internals

data Konto = Konto { played :: Int, points :: Int } 
           deriving ( Eq, Show )

$(deriveSafeCopy 0 'base ''Konto)
  
$(asXmlRpcStruct ''Konto)  
  
instance Num Konto where 
    k + i = Konto { played = played k + played i 
                  , points = points k + points i 
                  }

newtype Bank = Bank ( M.Map Name Konto ) 
             deriving Typeable

$(deriveSafeCopy 0 'base ''Bank)

instance ( XmlRpcType v ) => XmlRpcType ( M.Map String v ) where
  getType _ = TStruct
  toValue m = ValueStruct $ M.toList $ M.map toValue m
  
instance XmlRpcType Bank where
  getType _ = TStruct
  toValue ( Bank m ) = toValue $ M.mapKeys show m

empty :: Bank
empty = Bank M.empty

        
updates :: [(Name, Int, Int)] -> Update Bank ()
updates ts = do
    Bank previous <- get
    put $ Bank
        $ M.unionWith (+) previous
        $ M.fromListWith (+)
        $ map ( \ (n,pl,pt) -> 
               (n, Konto { played = pl, points = pt }))
        $ ts

snapshot :: Query Bank Bank
snapshot = ask

$(makeAcidic ''Bank [ 'updates, 'snapshot ])
  
pretty (Bank b) = text "statistics:" <+> vcat 
        ( map ( text . show ) 
        $ sortBy ( comparing ( points . snd ) ) 
        $ M.toList b 
        )
