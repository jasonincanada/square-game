{-# Language TemplateHaskell #-}

module SquareGame.World where

import qualified Data.Map as M
import           Control.Lens
import           Graphics.Gloss (Picture(Blank))
import           SquareGame

data World = World { _board     :: Board
                   , _message   :: String
                   , _steps     :: Int
                   , _time      :: Float
                   , _cellHover :: Maybe Cell

                   -- The set of shrouded Cells to highlight as a preview of what would be
                   -- revealed when the user clicks
                   , _cellsToClick :: Maybe CellSet

                   -- Cache our last render of the board so we don't recompute redundantly every tick
                   , _rendered    :: Picture
                   , _renderCount :: Int

                   -- The size of the square we're placing, if any
                   , _placing       :: Maybe Size
                   , _squareToPlace :: Maybe Square
                   , _placed        :: [Square]

                   -- The number of placeable squares left of each size
                   , _placeables    :: M.Map Size Int

                   -- Which placed square the cursor is over
                   , _squareToPickup :: Maybe Square

                   -- Map of soonest times an event can occur again
                   , _debounces :: M.Map String Float
                   }

makeLenses ''World

makeWorld :: Board -> World
makeWorld board = World board
                        "starting message"
                        0
                        0.0
                        Nothing
                        Nothing
                        Blank
                        0
                        Nothing
                        Nothing
                        []
                        M.empty
                        Nothing
                        M.empty
