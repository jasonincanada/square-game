{- Partridge Squares -}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import SquareGame

main :: IO ()
main = do
  Board squares cells <- fromFile "generation/squares/N8-888666688445522333178876768555777744.sqr"
  print $ M.keys squares

{-  λ> main
    [(0,0,8),(0,8,8),(0,16,8),(0,24,6),(0,30,6),(6,24,6),(6,30,6),(8,0,8),(8,8,8),(8,16,4),(8,20,4),(12,16,5),(12,21,5),(12,26,2),(12,28,2),(12,30,3),(12,33,3),(14,26,3),(14,29,1),(15,29,7),(16,0,8),(16,8,8),(17,16,7),(17,23,6),(22,29,7),(23,23,6),(24,0,8),(24,8,5),(24,13,5),(24,18,5),(29,8,7),(29,15,7),(29,22,7),(29,29,7),(32,0,4),(32,4,4)]
-}
