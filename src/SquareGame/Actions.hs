module SquareGame.Actions (
    leftClick
   
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Arrow ((>>>))
import           Control.Lens
import           Control.Monad.State
import           SquareGame
import           SquareGame.World

-- User clicked, so show the cells we've already determined can be deshrouded (during mouseover),
-- then do to the three types of auto-revealing until there are no further changes to do
leftClick :: World -> World
leftClick = execState click
  where
    click :: State World ()
    click = do
      clicked <- gets $ view cellsToClick

      modify $ over board $ deshroudCells clicked
                            >>> execState (sweepBoard clicked)

      modify $ set cellsToClick S.empty


    sweepBoard :: CellSet -> State Board ()
    sweepBoard cells = do
      grid <- gets $ view grid

      -- Since we're using Data.Set's map, this will de-dupe the resulting list for us,
      -- which may leave the set smaller than the list--this is fine
      let affectedSquares = S.map (\cell -> fst $ grid M.! cell) cells

      mapM_ sweepSquare affectedSquares


    sweepSquare :: Square -> State Board ()
    sweepSquare square = do

      before <- shroudsize square
      sweepEdges square
      sweepEight square
      sweepBorder square
      after <- shroudsize square

      -- Keep recursing until a round of sweeping has no effect
      when (before /= after) (sweepSquare square)


    shroudsize :: Square -> State Board Int
    shroudsize square = do
      squares <- gets $ view squares
      let shroud = fst $ squares M.! square
      return $ S.size shroud


    sweepEdges :: Square -> State Board ()
    sweepEdges square = sweep square STop
                     >> sweep square SRight
                     >> sweep square SBottom
                     >> sweep square SLeft


    sweep :: Square -> SquareSide -> State Board ()
    sweep square edge = do
      squares <- gets $ view squares

      let cellsets = squares M.! square
      let swept    = sweepEdge square edge cellsets

      modify $ deshroudCells swept


    sweepEight :: Square -> State Board ()
    sweepEight square = do
      squares <- gets $ view squares

      let cellsets   = squares M.! square
      let unveilable = any (sweepableEight square cellsets) [STop, SRight, SBottom, SLeft]

      when unveilable (modify $ deshroudCells (fst cellsets))


    sweepBorder :: Square -> State Board ()
    sweepBorder square = do
      squares <- gets $ view squares

      let shroud = fst $ squares M.! square
      let swept  = borderShroud shroud

      modify $ deshroudCells swept
