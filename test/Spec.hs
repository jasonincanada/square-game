{- Test suite for SquareGame -}

import SquareGame
import Test.Hspec

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
      click (2,4,2) STop    `shouldBe` ( [(3,9),(3,10)]  :: [Cell] )

    it "right side" $
      click (2,4,2) SRight  `shouldBe` ( [(5,12),(6,12)] :: [Cell] )

    it "bottom side" $
      click (2,4,2) SBottom `shouldBe` ( [(8,9),(8,10)]  :: [Cell] )

    it "left side" $
      click (2,4,2) SLeft   `shouldBe` ( [(5,7),(6,7)]   :: [Cell] )


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
      expand ((0,0), 3) STop    `shouldBe` [ (0,0), (0,1), (0,2),
                                             (1,0), (1,1), (1,2),
                                             (2,0), (2,1), (2,2) ]
    it "bottom side 3-cell deshroud" $
      expand ((2,0), 3) SBottom `shouldBe` [ (2,0), (2,1), (2,2),
                                             (1,0), (1,1), (1,2),
                                             (0,0), (0,1), (0,2) ]
    it "left side 3-cell deshroud" $
      expand ((0,0), 3) SLeft   `shouldBe` [ (0,0), (0,1), (0,2),
                                             (1,0), (1,1), (1,2),
                                             (2,0), (2,1), (2,2) ]
    it "right side 3-cell deshroud" $
      expand ((0,2), 3) SRight  `shouldBe` [ (0,2), (0,1), (0,0),
                                             (1,2), (1,1), (1,0),
                                             (2,2), (2,1), (2,0) ]

