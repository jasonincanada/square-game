{- Test suite for SquareGame -}

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Function ((&))
import SquareGame
import Test.Hspec

-- These would be read from a file/database. This is our mock board, a mini 4x6 board that
-- has one 4x4 square to the left of two stacked 2x2 squares
testSquares :: [Square]
testSquares = [(0,0,4), (0,4,2), (2,4,2)]

main :: IO ()
main = hspec $ do

  describe "borderCells computes the cells for..." $ do
    it "top border" $
      borderCells (0,0,2) STop    `shouldBe` ( [(0,0),(0,1),(0,2),(0,3)] :: [Cell] )

    it "right border" $
      borderCells (0,0,2) SRight  `shouldBe` ( [(0,3),(1,3),(2,3),(3,3)] :: [Cell] )

    it "bottom border" $
      borderCells (0,0,2) SBottom `shouldBe` ( [(3,0),(3,1),(3,2),(3,3)] :: [Cell] )

    it "left border" $
      borderCells (0,0,2) SLeft   `shouldBe` ( [(0,0),(1,0),(2,0),(3,0)] :: [Cell] )


  describe "click computes the cells to reveal for side..." $ do
    it "top side" $
      click (2,4,2) STop    `shouldBe` ( S.fromList [(3,9),(3,10)]  :: CellSet )

    it "right side" $
      click (2,4,2) SRight  `shouldBe` ( S.fromList [(5,12),(6,12)] :: CellSet )

    it "bottom side" $
      click (2,4,2) SBottom `shouldBe` ( S.fromList [(8,9),(8,10)]  :: CellSet )

    it "left side" $
      click (2,4,2) SLeft   `shouldBe` ( S.fromList [(5,7),(6,7)]   :: CellSet )


  describe "cells maps a square to its cells" $ do
    it "1x1 square in top-left corner" $
      cells (0,0,1) `shouldBe` ( [((0,0),CTopLeft    ),
                                  ((0,1),CTopRight   ),
                                  ((1,0),CBottomLeft ),
                                  ((1,1),CBottomRight)] :: [(Cell,CellBorder)] )

    it "3x3 square at row 2, column 4" $
      cells (2,4,3) `shouldBe`
        ( [ -- top
            ((4,8),CTopLeft),((4,9),CTop),((4,10),CTop),((4,11),CTop),((4,12),CTop),((4,13),CTopRight),

            -- left side
            ((5,8),CLeft),((6,8),CLeft),((7,8),CLeft),((8,8),CLeft),

            -- interior cells with no border
            ((5,9),CNone),((5,10),CNone),((5,11),CNone),((5,12),CNone),
            ((6,9),CNone),((6,10),CNone),((6,11),CNone),((6,12),CNone),
            ((7,9),CNone),((7,10),CNone),((7,11),CNone),((7,12),CNone),
            ((8,9),CNone),((8,10),CNone),((8,11),CNone),((8,12),CNone),

            -- right side
            ((5,13),CRight),((6,13),CRight),((7,13),CRight),((8,13),CRight),

            -- bottom
            ((9,8),CBottomLeft),((9,9),CBottom),((9,10),CBottom),((9,11),CBottom),((9,12),CBottom),((9,13),CBottomRight) ]
            :: [(Cell,CellBorder)]
        )

    it "returns 5184 cells for a square-sized sub-square" $
      length (cells (0,0,36)) `shouldBe` (36*36*4)


  describe "contigs" $ do
    it "on empty list" $
      contigs (next STop) [] `shouldBe` []

    it "on singleton list" $
      contigs (next STop) [(0,0)] `shouldBe` [((0,0),1)]

    it "on top side cells" $
      contigs (next STop) [(0,0),(0,1),(0,2), (0,100),(0,101), (0,200)] 
        `shouldBe` [ ((0,0  ), 3),
                     ((0,100), 2),
                     ((0,200), 1)
                   ]

    it "on left side cells" $
      contigs (next SLeft) [(0,0),(1,0),(2,0), (100,0),(101,0), (200,0)] 
        `shouldBe` [ ((0,0  ), 3),
                     ((100,0), 2),
                     ((200,0), 1)
                   ]


  describe "expand" $ do
    it "top side 3-cell deshroud" $
      expand STop ((0,0), 3)    `shouldBe` [ (1,0), (1,1), (1,2),
                                             (2,0), (2,1), (2,2) ]

    it "bottom side 3-cell deshroud" $
      expand SBottom ((2,0), 3) `shouldBe` [ (1,0), (1,1), (1,2),
                                             (0,0), (0,1), (0,2) ]

    it "left side 3-cell deshroud" $
      expand SLeft ((0,0), 3)   `shouldBe` [ (0,1), (0,2),
                                             (1,1), (1,2),
                                             (2,1), (2,2) ]
    it "right side 3-cell deshroud" $
      expand SRight ((0,2), 3)  `shouldBe` [ (0,1), (0,0),
                                             (1,1), (1,0),
                                             (2,1), (2,0) ]


  describe "board" $ do
    let (Board squares cells) = makeBoard testSquares

    it "maps squares to grid cells" $
      squares `shouldBe` M.fromList [ ((0,0,4),( S.fromList [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7),(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7)]
                                               , S.fromList []))
                                    , ((0,4,2),( S.fromList [(0,8),(0,9),(0,10),(0,11),(1,8),(1,9),(1,10),(1,11),(2,8),(2,9),(2,10),(2,11),(3,8),(3,9),(3,10),(3,11)]
                                               , S.fromList []))
                                    , ((2,4,2),( S.fromList [(4,8),(4,9),(4,10),(4,11),(5,8),(5,9),(5,10),(5,11),(6,8),(6,9),(6,10),(6,11),(7,8),(7,9),(7,10),(7,11)]
                                               , S.fromList []))
                                    ]

    it "maps the first cell to the first square" $
      cells M.! (0,0)  `shouldBe` ((0,0,4), CTopLeft)

    it "maps the last cell to the last square" $
      cells M.! (7,11) `shouldBe` ((2,4,2), CBottomRight)

    it "maps to the right number of cells" $
      M.size cells `shouldBe` 4*(4*4 + 2*2 + 2*2)


  describe "borderShroud" $ do
    let top2rows    = S.fromList [(0, 0),(0, 1),(0, 2), (1, 1)]
    let bottom2rows = S.fromList [(71,0),(71,1),(71,2), (70,1)]
    let left2cols   = S.fromList [(0, 0),(1, 0),(2, 0), (1, 1)]
    let right2cols  = S.fromList [(0,71),(1,71),(2,71), (1,70)]

    it "deshrouds the top" $
      borderShroud top2rows    `shouldBe` S.fromList [(0,0),(0,2)]

    it "deshrouds the bottom" $
      borderShroud bottom2rows `shouldBe` S.fromList [(71,0),(71,2)]

    it "deshrouds the left" $
      borderShroud left2cols   `shouldBe` S.fromList [(0,0),(2,0)]

    it "deshrouds the right" $
      borderShroud right2cols  `shouldBe` S.fromList [(0,71),(2,71)]


  describe "deshroud" $ do
    let Board squares _ = deshroud (0,4,2) $ makeBoard testSquares

    it "deshrouds a square" $
      squares M.! (0,4,2) `shouldBe` ( -- Nothing left in the set of shrouded cells
                                       S.empty,

                                       -- They've all been moved to the unshrouded set
                                       S.fromList [(0,8),(0,9),(0,10),(0,11),
                                                   (1,8),(1,9),(1,10),(1,11),
                                                   (2,8),(2,9),(2,10),(2,11),
                                                   (3,8),(3,9),(3,10),(3,11)])


  describe "sweepEdge" $ do
    let Board squares grid = makeBoard testSquares &
                               deshroudCells (S.fromList [ (0,0), (0,1), (0,2) ])

    it "sweeps a cube from the top edge" $
      sweepEdge (0,0,4) STop (squares M.! (0,0,4)) `shouldBe` S.fromList [ (1,0),(1,1),(1,2)
                                                                         , (2,0),(2,1),(2,2) ]

