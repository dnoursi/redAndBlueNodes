import Data.Bool

--Tuple functions

--map 3-tuple ie: fmap (map3t (3+)) red
map3t :: (Double->Double) -> (Double,Double,Double) -> (Double,Double,Double)
map3t f (x,y,z) = (f x, f y, f z)

-- for use to applicative map 3-tuple ie: (maps3t (-)) <$> red <*> blue
maps3t :: (Double->Double->Double) -> (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
maps3t f (x1,y1,z1) (x2,y2,z2) = (f x1 x2, f y1 y2, f z1 z2)

maps3tg :: (Double->Double->Double) -> [(Double,Double,Double)] -> [(Double,Double,Double)]  -> [(Double,Double,Double)]
maps3tg f ls lz= [(x2-x1,y2-y1,z2-z1) | (x1,y1,z1) <- ls, (x2, y2, z2) <- lz]

goodmaps3t :: (Double->Double->Double) -> [(Double,Double,Double)] -> [(Double,Double,Double)]  -> [(Double,Double,Double)]
goodmaps3t f ((x1,y1,z1):ls) ((x2,y2,z2):lz) = (f x1 x2, f y1 y2, f z1 z2):(goodmaps3t f ls lz)
goodmaps3t f [] [] = []

--Solution
red=[(1.0,1.0,10.0),(3.0,5.0,7.0),(0.0,2.0,6.0)]
blue=[(2.0,2.0,20.0),(3.0,3.0,30.0),(4.0,-1.0,6.0)]
--eg: > compute red blue
parallel :: (Double,Double,Double) -> (Double,Double,Double) -> Bool
parallel (x1,y1,z1) (x2,y2,z2) = x1/x2==y1/y2 && y1/y2==z1/z2

--main call: to run, call "compute red blue"
--compute takes the first element of its first argument, and finds it a match ("findMatch") in its second argument
compute :: [(Double,Double,Double)] -> [(Double,Double,Double)]  -> [( (Double,Double,Double) , (Double,Double,Double))]
compute (l:ls) lz = ( l , fst(findMatch l lz) ):(compute ls (snd(findMatch l lz)))
compute [] [] = []

-- new lz  =   (snd(findMatch l lz))
-- new l:ls  = ls
-- valid pair  = ( l , fst(findMatch l lz) )

--goes through its list argument, and tests if each element will work or create a line that hits another point
findMatch :: (Double,Double,Double) -> [(Double,Double,Double)]  -> ((Double,Double,Double),[(Double,Double,Double)])
findMatch x (k:l:ls) = if (satisfies x k (l:ls)) then (k,l:ls) else (findMatch x ( (l:ls)++[k] ))
findMatch _ (l:ls) = (l,ls)

--ensures that no points in ls will be intersected by a line connecting x and l
satisfies :: (Double,Double,Double) -> (Double,Double,Double) -> [(Double,Double,Double)] -> Bool
satisfies x l ls =  all (==False) (fmap (parallel (maps3t (-) x l)) ls)
