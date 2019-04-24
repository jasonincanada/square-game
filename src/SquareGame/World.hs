{-# Language TemplateHaskell #-}

module SquareGame.World where

import Control.Lens
import Graphics.Gloss (Picture)
import SquareGame

data World = World { _board     :: Board
                   , _message   :: String
                   , _cellHover :: Maybe Cell

                   -- The set of shrouded Cells to highlight as a preview of what would be
                   -- revealed when the user clicks
                   , _cellsToClick :: CellSet

                   -- Cache our last render of the board so we don't recompute redundantly every tick
                   , _rendered    :: Picture
                   , _renderCount :: Int

                   -- The size of the square we're placing, if any
                   , _placing :: Maybe Size
                   }

makeLenses ''World
