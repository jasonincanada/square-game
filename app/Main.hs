{- Partridge Squares -}

module Main where

import qualified Data.Set as S
import           Control.Lens
import           Data.Char (digitToInt)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact
import           SquareGame
import           SquareGame.Actions
import           SquareGame.Render
import           SquareGame.World

file :: FilePath
file = "generation/squares/N8-888666688445522333178876768555777744.sqr"

main :: IO ()
main = do
  board <- fromFile file

  let started = randomDeshroud 1 board
  let clicked = click (0,0,8) SRight
  let world   = World
                  started
                  "default message"
                  Nothing
                  S.empty
                  Blank
                  0
                  Nothing

  let withCache = world & rendered .~ render world

  play window white 10 withCache displayBoard events step


window :: Display
window = InWindow "Partridge Square" size position
  where
    size         = (windowWidth, windowHeight)
    position     = (100, 100)


displayBoard :: World -> Picture
displayBoard world = world ^. rendered


events :: Event -> World -> World
events event world = case processEvent event world of
                       Just world' -> world' & rendered .~ render world'
                                             & renderCount %~ (+1)

                       Nothing     -> world


-- Return (Just world) if it requires a re-render or Nothing if not
processEvent :: Event -> World -> Maybe World
processEvent event world = case event of

  EventMotion (x, y)                         -> Just $ mouseMove (x, y) world

  EventKey (MouseButton WheelUp   ) _    _ _ -> Just $ wheelUp world
  EventKey (MouseButton WheelDown ) _    _ _ -> Just $ wheelDown world
  EventKey (MouseButton LeftButton) Down _ _ -> Just $ leftClick world

  EventKey (Char '0'              ) Down _ _ -> Just $ clearPlacingSquare world
  EventKey (Char  c               ) Down _ _ -> if c `elem` "12345678"
                                                then Just $ digitPress (digitToInt c) world
                                                else Nothing

  otherwise                                  -> Nothing


step :: Float -> World -> World
step float = id
