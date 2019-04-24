module SquareGame.Actions (
    leftClick
  , clearPlacingSquare
  , digitPress
  , mouseMove
  , wheelUp
  , wheelDown
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Arrow ((>>>))
import           Control.Lens
import           Control.Monad.State
import           SquareGame
import           SquareGame.Render (windowToCell, windowToSquareEdge)
import           SquareGame.World


-- User typed 0 so clear the placing square
clearPlacingSquare :: World -> World
clearPlacingSquare = set placing Nothing

-- User typed 1,2,..8, so set the size of placing square to it
digitPress :: Int -> World -> World
digitPress size = set placing (Just size)


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


mouseMove :: (Float, Float) -> World -> World
mouseMove (x, y) world = world & message      .~ show (world ^. placing)
                               & cellHover    .~ windowToCell x y
                               & cellsToClick .~ clickables world x y

clickables :: World -> Float -> Float -> CellSet
clickables world x y = cells
  where
    squares' = world ^. board ^. squares
    cells = case windowToSquareEdge world x y of
              Nothing             -> S.empty
              Just (square, edge) -> if fullyRevealed square
                                     then getFor square edge
                                     else S.empty

    fullyRevealed :: Square -> Bool
    fullyRevealed square = S.empty == fst (squares' M.! square)

    getFor :: Square -> SquareSide -> CellSet
    getFor square edge = intersect
      where
        intersect = S.intersection all shrouded
        all       = click square edge
        shrouded  = foldr S.union S.empty (M.elems $ M.map fst squares')


wheelUp :: World -> World
wheelUp world   = world & case world ^. placing of
                            Nothing   -> placing .~ Just 2
                            Just 8    -> id
                            Just size -> placing .~ Just (size+1)

wheelDown :: World -> World
wheelDown world = world & case world ^. placing of
                            Nothing -> placing .~ Just 2
                            Just 1  -> id
                            Just x  -> placing .~ Just (x-1)
