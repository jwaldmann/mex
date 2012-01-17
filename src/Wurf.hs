{-# language GeneralizedNewtypeDeriving #-}

module Wurf where


import Network.XmlRpc.Internals
import Control.Monad.Error

import System.Random
import Test.SmallCheck

newtype Augen = Augen Int deriving (Show, Eq, Ord, Num, Random)

instance Serial Augen where
    series d = do a <- [ 1 .. min d 6 ] ; return $ Augen a

instance XmlRpcType Augen where
    getType w = TInt
    toValue ( Augen a ) = toValue a
    fromValue v = do a <- fromValue v ; return $ Augen a

data Wurf = Wurf Augen Augen deriving ( Eq, Show )

wurf :: Augen -> Augen -> Wurf
wurf x1 x2 = if x1 >= x2 then Wurf x1 x2 else Wurf x2 x1

instance XmlRpcType Wurf where
  getType w = TArray
  toValue (Wurf x y) = 
    ValueArray [toValue x, toValue y ]
  fromValue v = case v of
      ValueArray [x,y] -> do
          a <- fromValue x                
          b <- fromValue y     
          return $ wurf a b
      _ -> ErrorT $ return $ Left "Wurf"    

instance Serial Wurf where
    series = cons2 wurf

instance Enum Wurf where
    succ w @ (Wurf x y) = 
        if x > y+1 then wurf x (y+1)
        else if x == y+1 then if x == 2 then error $ "no successor for " ++ show w 
                              else if x == 6 then wurf 1 1
                              else wurf (x+1) 1
        else if x == y then if x < 6 then wurf (x+1) (x+1) else wurf 2 1
        else error "huh"

instance Bounded Wurf where
    minBound = wurf 3 1
    maxBound = wurf 2 1


transitive r = \ u v w -> ( r u v && r v w ) <= r u w
antisymmetric r = \ x y -> ( r x y && r y x ) <= ( x == y )

roll :: IO Wurf
roll = do
    x <- randomRIO ( 1, 6 )
    y <- randomRIO ( 1, 6 )
    return $ wurf x y

instance Ord Wurf where
  compare x @ (Wurf a b) y @ (Wurf c d) = 
      if a == 2 && b == 1 then if x == y then EQ else GT
      else if c == 2 && d == 1 then LT
      else if a == b then if c == d then compare a c
                          else GT
      else if c == d then LT
      else compare (10 * a + b)(10 * c + d)
      