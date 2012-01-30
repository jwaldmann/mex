{-# language DeriveDataTypeable, TemplateHaskell #-}
{-# language TypeFamilies #-}

module Chart where

import qualified Bank 
import qualified Spieler

import Data.Time
import Data.Time.LocalTime
import Data.SafeCopy
import Data.Typeable
import Data.Acid
import Control.Monad.State
import Control.Monad.Reader ( ask )

import Data.Colour.Names
import Data.Colour
import Data.Colour.SRGB
import Data.Accessor
import Graphics.Rendering.Chart

import qualified Data.Map as M
import Control.Monad ( void )

-- | minimum interval between chart checkpoints, in seconds
chart_interval = 5 * 60
chart_location = "chart.png"

newtype Chart = Chart [ (UTCTime, Bank.Bank) ]
    deriving ( Typeable )

$(deriveSafeCopy 0 'base ''Chart)

empty :: Chart
empty = Chart []

add :: UTCTime -> Bank.Bank -> Update Chart ()
add time b = do
    Chart entries <- get
    put $ Chart $ (time,b) : entries

snapshot :: Query Chart Chart
snapshot = ask

$(makeAcidic ''Chart [ 'add, 'snapshot ])


names (Chart ch) = M.keys $ foldr M.union M.empty 
                 $ do ( t, Bank.Bank n ) <- ch ; return n

coloured xs = do
    let n = length xs
    (i, x) <- zip [0 .. ] xs
    let t :: Double
        t = fromIntegral i / fromIntegral (n-1)
        r = t
        g = 1 - t
        ( _, b ) = properFraction $ 4 * t
    return ( x, sRGB r g b )

local t = utcToLocalTime ( read "CET" ) t

curve (Chart ch) (n @ ( Spieler.Name s), c) = id
           $ plot_lines_style .> line_color ^= opaque c
           $ plot_lines_values ^= ( return $ do
               ( t, Bank.Bank b ) <- ch
               Just k <- return $ M.lookup n b
               return ( local t , Bank.rating k )  )
           $ plot_lines_title ^= s
           $ defaultPlotLines

ratings t ch = layout1_title ^= 
             ( "Ratings Chart " ++ "("++ show t ++ ")" )
         $ layout1_left_axis ^: laxis_override ^= axisGridHide
         $ layout1_right_axis ^: laxis_override ^= axisGridHide
         $ layout1_bottom_axis ^: laxis_override ^= axisGridHide
         $ layout1_plots ^= ( do nc <- coloured ( names ch )
                                 return $ Left $ toPlot $ curve ch nc )
         $ layout1_grid_last ^= False
         $ defaultLayout1

write ch = void $ do
    now <- getCurrentTime
    renderableToPNGFile 
      ( toRenderable $ ratings ( local now ) ch ) 
      800 600 chart_location
    