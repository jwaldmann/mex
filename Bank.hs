module Bank where

import Spieler

import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ
import Data.List ( sortBy )
import Data.Ord ( comparing )

data Konto = Konto { played :: Int, points :: Int } deriving ( Eq, Show )

instance Num Konto where 
    k + i = Konto { played = played k + played i , points = points k + points i }

type Bank = M.Map Name Konto

update :: [ Name ] -> (Name, Int) -> Bank -> Bank
update ns (n, delta) previous = 
      M.insertWith (+) n ( Konto { played = 0, points = delta } )
    $ M.unionWith (+) previous
    $ M.fromList ( zip ns $ repeat $ Konto { played = 1, points = 0 } )

pretty :: Bank -> Doc
pretty b = text "statistics:" <+> vcat 
    ( map ( text . show ) 
    $ sortBy ( comparing ( points . snd ) ) 
    $ M.toList b 
    )
