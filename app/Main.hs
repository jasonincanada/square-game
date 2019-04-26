{- Partridge Squares -}

module Main where

import qualified Data.Set as S
import           Control.Lens
import           Control.Monad.State
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

  let started   = randomDeshroud 1 board
  let clicked   = click (0,0,8) SRight
  let world     = makeWorld started
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
                       (True, world') -> world' & rendered .~ render world'
                                                & renderCount %~ (+1)

                       (_   , world') -> world'


processEvent :: Event -> World -> (Bool, World)
processEvent event world = runState state world
  where
    state = case event of

      EventMotion (x, y)                         -> mouseMove (x, y)

      EventKey (MouseButton WheelUp   ) _    _ _ -> wheelUp
      EventKey (MouseButton WheelDown ) _    _ _ -> wheelDown
      EventKey (MouseButton LeftButton) Down _ _ -> leftClick

      EventKey (Char '0'              ) Down _ _ -> clearPlacingSquare
      EventKey (Char  c               ) Down _ _ -> if c `elem` "12345678"
                                                    then digitPress (digitToInt c)
                                                    else return False

      otherwise                                  -> return False


step :: Float -> World -> World
step seconds world = case runState (advance seconds) world of
                       (True, world') -> world' & rendered .~ render world'
                                                & renderCount %~ (+1)

                       (_   , world') -> world'
