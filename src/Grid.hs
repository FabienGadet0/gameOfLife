module Grid where
import Graphics.Gloss

----------------------------      CELL       -------------------------------------------------

data Cell = Cell {
  x :: Int,
  y :: Int,
  realX :: Float,
  realY :: Float,
  state :: Int
} deriving(Eq, Show)

intToCol :: (Num a, Eq a) => a -> Picture -> Picture
intToCol 0 = Color black
intToCol 1 = Color orange

stateToShape :: (Num a, Eq a) => a -> Picture
stateToShape 0 = rectangleSolid (fromIntegral offset)  (fromIntegral offset)
stateToShape 1 = rectangleWire (fromIntegral offset)  (fromIntegral offset)
stateToShape a = rectangleSolid (fromIntegral offset)  (fromIntegral offset)

convertToRealx :: Int -> Float
convertToRealx x = fromIntegral $ (-(winx `div` 2)) + (offset * x)

convertToRealy :: Int -> Float
convertToRealy y = fromIntegral $ (winy `div` 2) - (offset * y)

makeCell :: (Int, Int) -> Int -> Cell
makeCell (x,y) state = Cell x y (convertToRealx x) (convertToRealy y) state

----------------------------      CONST       -------------------------------------------------

winx :: Int
winx = 800

winy :: Int
winy = 800

offset :: Int
offset = 8

----------------------------      Scene + Event       -------------------------------------------------
event :: t1 -> t -> t
event _ world = world

render :: [Cell] -> Picture
render pts = Pictures $ fmap f pts
  where f (Cell _ _  x y state) = translate x y ((intToCol state) (stateToShape state))

grid2d :: [Cell]
grid2d = concat [ [ makeCell(x,y) 0 | x <- [1..(fromIntegral winx `div` offset) - 1]] | y <- [1..(winy `div` offset) - 1]]

updateGrid :: ((Int, Int) -> Int) -> [Cell]
updateGrid f = concat [ [ makeCell(x,y) (f(x,y)) | x <- [1..(fromIntegral winx `div` offset) - 1]] | y <- [1..(winy `div` offset) - 1]]
