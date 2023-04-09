module Queens (boardString, canAttack) where

import Data.List

type Column = Int
type Line = Int

data BoardElement = 
    Square Line Column |
    Space |
    LineEnd

boardString :: Maybe (Line, Column) -> Maybe (Line, Column) -> String
boardString white black = map (renderWithQueens white black) emptyBoard

-- an "abstract" representation of an empty board including lineends and spaces between squares.
emptyBoard :: [BoardElement]
emptyBoard = concat $ [ singleLine li | li <- [0..7] ]
  where
    singleLine li = (++ [LineEnd]) . (intersperse Space) $ [ Square li col | col <- [0..7] ]

renderWithQueens :: Maybe (Line, Column) -> Maybe (Line, Column) -> BoardElement -> Char
renderWithQueens _ _ Space = ' '
renderWithQueens _ _ LineEnd = '\n'
renderWithQueens white _ square | sameCoordinates white square = 'W'
renderWithQueens _ black square | sameCoordinates black square = 'B'
renderWithQueens _ _ (Square _ _) = '_'

sameCoordinates :: Maybe(Line, Column) -> BoardElement -> Bool
sameCoordinates (Just (liQ, colQ)) (Square liS colS) = liQ == liS && colQ == colS
sameCoordinates _ _ = False

canAttack :: (Line, Column) -> (Line, Column) -> Bool
canAttack (liA, colA) (liB, colB) = liA == liB || colA == colB || liDist == colDist
  where
    -- if the line distance equals the column distance the queens are diagonal
    liDist = abs $ liA - liB
    colDist = abs $ colA - colB
