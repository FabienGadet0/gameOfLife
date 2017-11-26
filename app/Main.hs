import Grid as G
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import qualified Data.Set as S

neigh ::(Enum t, Enum t1, Num t, Num t1, Ord t, Ord t1) => (t1, t) -> S.Set (t1, t)
neigh (x0, y0) = S.fromList [ (x, y) | x <- [x0 - 1 .. x0 + 1], y <- [y0 - 1 .. y0 + 1], x0 /= x || y0 /= y]

nbofNeigh ::
  (Num t1, Num t, Enum t1, Enum t, Ord t, Ord t1) =>
  (t1, t) -> S.Set (t1, t) -> Int
nbofNeigh cell glider =  (S.size $ S.intersection (neigh cell) glider)

setAlive ::
  (Ord t1, Ord t, Enum t, Enum t1, Num t, Num t1) =>
  (t1, t) -> S.Set (t1, t) -> Bool
setAlive cell glider = S.member cell glider && neighbours == 2  || neighbours == 3 where neighbours = nbofNeigh cell glider

oneTurn ::
  (Num t1, Num t, Enum t1, Enum t, Ord t, Ord t1) =>
  S.Set (t1, t) -> [(t1, t)]
oneTurn glider = do
  let relevantNeighbor = foldMap neigh glider
  filter foo (S.toList relevantNeighbor) where foo cell = setAlive cell glider

step :: S.Set (Int, Int) -> S.Set (Int, Int)
step x = S.fromList $ oneTurn x

existingCell :: Ord a => a -> S.Set a -> Bool
existingCell cell glider = S.member cell glider

dispayGame ::
  (Ord t, Ord t1, Enum t1, Enum t, Num t1, Num t) =>
  S.Set (t1, t) -> [Char]
dispayGame glider = 
    unlines [ [ if (existingCell (x,y) glider ) then '*' else ' ' | x <- [0..50]] | y <- [0..50]]

toLogic :: S.Set (Int, Int) -> [Cell]
toLogic glider = fmap (\a -> G.makeCell a 1) (S.toList glider)

toSet :: [Cell] -> S.Set (Int, Int)
toSet cells = S.fromList $ fmap (\a -> (x a, y a)) $ filter (\a -> state a == 1) cells

isItAlive :: S.Set (Int, Int) -> (Int, Int) -> Int
isItAlive glider cell | S.member cell glider = 1 | otherwise = 0
 
glider = S.fromList $ concat [[(x,y) | x <- [35..65]] | y <- [30..70]]

update :: ViewPort -> Float -> [Cell] -> [Cell]
update _ _ game = G.updateGrid (isItAlive (step (toSet game)))

initGrid = G.updateGrid (isItAlive(glider))

main =  simulate (InWindow "Game Of Life" (winx, winy) (500,150)) white 10
  initGrid  G.render update