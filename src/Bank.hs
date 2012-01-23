module Bank where

import Spieler

import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ
import Data.List ( sortBy )
import Data.Ord ( comparing )

data Konto = Konto { played :: Int, points :: Int } deriving ( Eq, Show )

instance Num Konto where 
    k + i = Konto { played = played k + played i , points = points k + points i }

newtype Bank = Bank ( M.Map Name Konto )

empty :: Bank
empty = Bank M.empty

update :: (Name, Int, Int) -> Bank -> Bank
update (n, pl, pt) ( Bank previous ) = 
      Bank
    $ M.insertWith (+) n ( Konto { played = pl, points = pt } )
    $ previous

pretty :: Bank -> Doc
pretty (Bank b) = text "statistics:" <+> vcat 
    ( map ( text . show ) 
    $ sortBy ( comparing ( points . snd ) ) 
    $ M.toList b 
    )
