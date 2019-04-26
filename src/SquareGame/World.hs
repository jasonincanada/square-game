{-# Language TemplateHaskell #-}

module SquareGame.World where

import Control.Lens
import Data.Set       (empty)
import Graphics.Gloss (Picture(Blank))
import SquareGame

data World = World { _board     :: Board
                   , _message   :: String
                   , _steps     :: Int
                   , _cellHover :: Maybe Cell

                   -- The set of shrouded Cells to highlight as a preview of what would be
                   -- revealed when the user clicks
                   , _cellsToClick :: CellSet

                   -- Cache our last render of the board so we don't recompute redundantly every tick
                   , _rendered    :: Picture
                   , _renderCount :: Int

                   -- The size of the square we're placing, if any
                   , _placing       :: Maybe Size
                   , _squareToPlace :: Maybe Square
                   , _placed        :: [Square]

                   -- Which placed square the cursor is over
                   , _squareToPickup :: Maybe Square
                   }

makeLenses ''World

makeWorld :: Board -> World
makeWorld board = World board
                        "starting message"
                        0
                        Nothing
                        Data.Set.empty
                        Blank
                        0
                        Nothing
                        Nothing
                        []
                        Nothing
