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

step ::
  (Enum t, Enum t1, Num t, Num t1, Ord t, Ord t1) =>
  S.Set (t1, t) -> S.Set (t1, t)
step x = S.fromList $ oneTurn x

existingCell :: Ord a => a -> S.Set a -> Bool
existingCell cell glider = S.member cell glider

dispayGame ::
  (Ord t, Ord t1, Enum t1, Enum t, Num t1, Num t) =>
  S.Set (t1, t) -> [Char]
dispayGame glider = 
    unlines [ [ if (existingCell (x,y) glider ) then '*' else ' ' | x <- [0..50]] | y <- [0..50]]

main :: IO ()
main = do
    let glider = S.fromList [(4,5),(4,6),(4,7)]
    mapM_ (putStrLn . dispayGame) (iterate step glider)
