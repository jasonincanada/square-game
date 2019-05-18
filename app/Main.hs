{- Partridge Squares -}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Lens
import           Control.Monad.State
import           Data.Char (digitToInt)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact
import           System.Environment (getArgs)
import           SquareGame
import           SquareGame.Actions
import           SquareGame.Render
import           SquareGame.UI     (windowWidth, windowHeight)
import           SquareGame.World

getSequences :: IO [String]
getSequences = lines <$> readFile "generation/squares/N8-sequences.txt"

fileFor :: Int -> IO String
fileFor n = do
  nth <- (!! (n-1)) <$> getSequences
  return ("generation/squares/N8-" ++ nth ++ ".sqr")


main :: IO ()
main = do
  boardID <- read . head <$> getArgs
  board   <- fileFor boardID >>= fromFile

  let ss        = M.keys (board ^. squares)
  let cleared   = foldr deshroud board ss

  --let started   = randomDeshroud 1 board
  --let clicked   = click (0,0,8) SRight
  let world     = makeWorld ("Board " ++ show boardID) cleared -- started
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

      EventMotion (x, y)                           -> mouseMove (x, y)

      EventKey (MouseButton WheelUp     ) _    _ _ -> debounce 100 "wheelUp" wheelUp
      EventKey (MouseButton WheelDown   ) _    _ _ -> debounce 100 "wheelDown" wheelDown
      EventKey (MouseButton LeftButton  ) Down _ _ -> leftClick
      EventKey (MouseButton MiddleButton) Down _ _ -> clearPlacingSquare

      EventKey (Char '0'                ) Down _ _ -> clearPlacingSquare
      EventKey (Char  c                 ) Down _ _ -> if c `elem` "12345678"
                                                      then digitPress (digitToInt c)
                                                      else return False

      otherwise                                    -> return False


step :: Float -> World -> World
step seconds world = case runState (advance seconds) world of
                       (True, world') -> world' & rendered .~ render world'
                                                & renderCount %~ (+1)

                       (_   , world') -> world'
