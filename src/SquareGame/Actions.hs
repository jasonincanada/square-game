{-# Language MultiWayIf #-}

module SquareGame.Actions (
    advance
  , clearPlacingSquare
  , debounce
  , digitPress
  , leftClick
  , mouseMove
  , wheelUp
  , wheelDown
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Arrow     ((>>>))
import           Control.Lens
import           Control.Monad.State
import           Data.List         (delete)
import           Data.Maybe        (isJust)
import           SquareGame
import           SquareGame.UI     (clickables, windowToCell)
import           SquareGame.World


-- A world action can modify the world and trigger a re-render by returning True
type WorldAction = State World Bool


-- User typed 0 so clear the placing square
clearPlacingSquare :: WorldAction
clearPlacingSquare = do
  modify $ set placing Nothing
  return True


-- User typed 1,2,..8, so set the size of placing square to it
digitPress :: Int -> WorldAction
digitPress size = do
  modify $ set placing (Just size)
  return True


leftClick :: WorldAction
leftClick = do
  placing <- gets $ view squareToPlace
  picking <- gets $ view squareToPickup
  cells   <- gets $ view cellsToClick

  if | isJust placing -> place
     | isJust picking -> pickUp
     | isJust cells   -> reveal
     | otherwise      -> return False


{- Place mode - place a square if it doesn't overlap with any already-revealed or placed square -}
place :: WorldAction
place = do
  Just square <- gets $ view squareToPlace
  overlapping <- any (square `overlaps`) <$> alreadyCovered

  if overlapping
    then modify $ set message $ "Can't place square at " ++ show square
    else modify $ over placed (square:)

  return True

  where
    alreadyCovered :: State World [Square]
    alreadyCovered = do
      squares <- gets $ view (board . squares)
      placed  <- gets $ view placed

      return $ placed ++ (  M.keys
                          $ M.filter (\cellsets -> S.empty == fst cellsets) squares)

    overlaps :: Square -> Square -> Bool
    overlaps s1 s2 = S.empty /= intersection
      where
        intersection = tiles s1 `S.intersection` tiles s2


pickUp :: WorldAction
pickUp = do
  Just square <- gets $ view squareToPickup

  let (_, _, size) = square

  modify $ set placing       (Just size)
  modify $ set squareToPlace (Just square)
  modify $ over placed       (delete square)
  return True


{- Reveal mode - reveal cells on the other side of the clicked edge -}
reveal :: WorldAction
reveal = do
  Just clicked <- gets $ view cellsToClick

  modify $ over board $ deshroudCells clicked
                        >>> execState (sweepBoard clicked)

  modify $ set cellsToClick Nothing

  return True

  where
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


mouseMove :: (Float, Float) -> WorldAction
mouseMove (x, y) = do
  toClick <- gets $ clickables x y . view board

  let cell = windowToCell x y   :: Maybe Cell

  modify $ set cellHover     cell
  modify $ set cellsToClick  toClick

  setPlacingSquare
  setPickingSquare

  return True


setPlacingSquare :: WorldAction
setPlacingSquare = do
  size <- gets $ view placing
  cell <- gets $ view cellHover

  modify $ set squareToPlace (clampedSquare <$> size <*> cell)

  return True

  where
    clampedSquare :: Size -> Cell -> Square
    clampedSquare size (crow, ccol) =
      let row = crow `div` 2 - size `div` 2
          col = ccol `div` 2 - size `div` 2
      in  clampSquare (row, col, size)


-- Remember the last placed square we moused over
setPickingSquare :: WorldAction
setPickingSquare = do
  cell   <- gets $ view cellHover
  placed <- gets $ view placed

  let square = join (find <$> cell <*> Just placed)

  modify $ set squareToPickup square

  return True

  where
    find :: Cell -> [Square] -> Maybe Square
    find _ [] = Nothing
    find cell (square : rest)
      | cell `isIn` square = Just square
      | otherwise          = find cell rest

    isIn :: Cell -> Square -> Bool
    isIn (crow,ccol) (srow,scol,size) =    r >= srow && r < (srow+size)
                                        && c >= scol && c < (scol+size)

                                        where r = crow `div` 2
                                              c = ccol `div` 2


wheelUp :: WorldAction
wheelUp = wheel up >> setPlacingSquare
  where
    up :: Maybe Size -> Maybe Size
    up Nothing     = Just 2
    up (Just 8)    = Just 8
    up (Just size) = Just (size + 1)

wheelDown :: WorldAction
wheelDown = wheel down >> setPlacingSquare
  where
    down :: Maybe Size -> Maybe Size
    down Nothing     = Nothing
    down (Just 1)    = Nothing
    down (Just size) = Just (size - 1)

wheel :: (Maybe Size -> Maybe Size) -> WorldAction
wheel f = do
  modify $ over placing f
  return True


debounce :: Float -> String -> WorldAction -> WorldAction
debounce delay event action = do
  map  <- gets $ view debounces
  time <- gets $ view time

  if event `M.member` map
    then
      if time >= map M.! event
        then nextFrom time >> action
        else return False

    else
      nextFrom time >> action

  where
    nextFrom :: Float -> State World ()
    nextFrom time = do
      let earliestTime = time + (delay / 1000)
      modify $ over debounces (M.insert event earliestTime)


advance :: Float -> WorldAction
advance seconds = do
  modify $ over time  (+seconds)
  modify $ over steps (+1)
  return False
