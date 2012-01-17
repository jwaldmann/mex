module Void where

import Network.XmlRpc.Internals
import Control.Monad.Error

instance XmlRpcType () where
    getType _ = TStruct
    toValue () = ValueStruct []
    fromValue v = case v of
        ValueStruct [] -> return ()
        _ -> ErrorT $ return $ Left "()"