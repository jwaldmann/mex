{-# language DeriveDataTypeable, TemplateHaskell #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}

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

data Konto = Konto { rating :: Double  
                   , played :: Int
                   , points :: Int 
                   , protocol_errors :: Int  
                   } 
           deriving ( Show )


$(deriveSafeCopy 0 'base ''Konto)
  
$(asXmlRpcStruct ''Konto)  
  

newtype Bank = Bank ( M.Map Name Konto ) 
             deriving ( Typeable, Show )

$(deriveSafeCopy 0 'base ''Bank)

instance ( XmlRpcType v ) => XmlRpcType ( M.Map String v ) where
  getType _ = TStruct
  toValue m = ValueStruct $ M.toList $ M.map toValue m
  fromValue v = case v of  
      ValueStruct kvs -> do
          pairs <- forM kvs $ \ (k,v) -> 
              do w <- fromValue v ; return ( k, w )
          return $ M.fromList pairs 
                                            

instance XmlRpcType Bank where
  getType _ = TStruct
  toValue ( Bank m ) = toValue $ M.mapKeys show m
  fromValue v = do m <- fromValue v ; return $ Bank $ M.mapKeys Name m

empty :: Bank
empty = Bank M.empty

-- | write the key-value-pairs into the bank,
-- replacing previous values associated to these keys
updates :: M.Map Name Konto -> Update Bank ()
updates m = do
    Bank previous <- get
    put $ Bank $ M.union m previous

snapshot :: Query Bank Bank
snapshot = ask

$(makeAcidic ''Bank [ 'updates, 'snapshot ])




total :: Bank -> Int
total (Bank b) = sum $ map points $ M.elems b
  
pretty (Bank b) =
    ( text "statistics, for" <+> text (show $ total $ Bank b) 
                              <+> text "total points" )
    $$ ( nest 3 $ vcat 
        $ map ( text . show ) 
        $ sortBy ( comparing ( negate . rating . snd ) ) 
        $ M.toList b 
        )
