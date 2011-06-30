module BarnesHut where

-- Barnes & Hut orbital simulator by Morten Olsen Lysgaard
-- mortenlysgaard.com

import Data.List (foldl', (\\))
import System.Random

data Vec = Vec Double Double deriving (Eq, Show)

vx (Vec x _) = x
vy (Vec _ y) = y

type BB = (Vec, Vec)

data Tree a = Branch BB (Vec,Vec,Double) (Tree a) (Tree a) (Tree a) (Tree a) | Leaf BB a | Empty BB deriving (Show, Eq)

obs (Empty _) = []
obs (Leaf _ a) = [a]
obs (Branch _ (p,s,m) _ _ _ _) = [Object { pos = p, speed=s, mass = m }]

firstLevelObjects :: Tree Object -> [Object]
firstLevelObjects (Branch _ _ a b c d) = concatMap obs [a,b,c,d]

data Object = Object { pos :: Vec
                     , speed :: Vec
                     , mass :: Double
                     } deriving (Show, Eq)

quad :: BB -> Object -> Int
quad (nw,se) o
  | x >= left
  , x <= (right - (width / 2))
  , y >= up
  , y <= (down - (height / 2))
  = 1
  | x >= (right - (width / 2))
  , x <= right
  , y >= up
  , y <= (down - (height / 2))
  = 2
  | x >= left
  , x <= (right - (width / 2))
  , y >= (down - (height / 2))
  , y <= down
  = 3
  | x >= (right - (width / 2))
  , x <= right
  , y >= (down - (height / 2))
  , y <= down
  = 4
  where p = pos o
        x = vx p
        y = vy p
        up = vy nw
        left = vx nw
        right = vx se
        down = vy se
        width = right - left
        height = down - up

firstQuad ((Vec left up), (Vec right down)) = (Vec left up, Vec (right - ((right - left)/2)) (down - ((down - up) / 2)))
secondQuad ((Vec left up), (Vec right down)) = (Vec (right - ((right - left)/2)) up, Vec right (down - ((down - up) / 2)))
thirdQuad ((Vec left up), (Vec right down)) = (Vec left (down - ((down - up) / 2)), Vec (right - ((right - left)/2)) down)
fourthQuad ((Vec left up), (Vec right down)) = (Vec (right - ((right - left)/2)) (down - ((down - up) / 2)), Vec right down)

calcBound (o:obj) = calcBound' (vx . pos $ o) (vx . pos $ o) (vy . pos $ o) (vy . pos $ o) obj
calcBound' l r u d [] = ((Vec l u),(Vec r d))
calcBound' left right up down (o:obj) = calcBound' nl nr nu nd obj
  where nl
         | (vx . pos $ o) < left = vx . pos $ o
         | otherwise = left
        nr
         | (vx . pos $ o) > right = vx . pos $ o
         | otherwise = right
        nu
         | (vy . pos $ o) < up = vy . pos $ o
         | otherwise = up
        nd
         | (vy . pos $ o) > down = vy . pos $ o
         | otherwise = down

buildTree planets = foldl' insertIn (Empty (calcBound planets)) planets

randPlanet = do
               x <- randomRIO (50,250)
               y <- randomRIO (50,250)
               xs <- randomRIO (-0.3,0.3)
               ys <- randomRIO (-0.3,0.3)
               let p = Vec x y
                   spd = Vec xs ys
                   origin = Vec 150 150
                   d = diff p origin
                   spinn = scale (unitify $ spn p origin d) 1
               m <- randomRIO (1,5)
               return Object {pos = (Vec x y), speed=spinn, mass=m }

spn p origin v
 | vx origin < vx p = Vec ((- vy v) / (vx v)) 1
 | otherwise = Vec ((vy v) / (vx v)) (-1)

randPlanets = sequence (replicate 300 randPlanet)

gamma = 0.0003

scale :: Vec -> Double -> Vec
scale (Vec x y) k = Vec (k * x) (k * y)

unitify v = scale v (1 / len v)

len :: Vec -> Double
len (Vec x y) = sqrt (x^2 + y^2)

add :: Vec -> Vec -> Vec
add (Vec a b) (Vec c d) = Vec (a+c) (b+d)

neg :: Vec -> Vec
neg (Vec a b) = Vec (-a) (-b)

sub :: Vec -> Vec -> Vec
sub v1 v2 = add v1 (neg v2)

angle :: Vec -> Double
angle (Vec x y) = atan (y/x)

angle' :: Vec -> Vec -> Double
angle' a b = angle (diff a b)

diff :: Vec -> Vec -> Vec
diff a b = b `sub` a

vsum :: [Vec] -> Vec
vsum vs = foldl' add (Vec 0 0) vs

forceOn :: Tree Object -> Object -> Vec
forceOn (Leaf _ a) o
  | a /= o = forceBetween (pos o) (mass o) (pos a) (mass a)
  | otherwise = Vec 0 0
forceOn (Empty _) _ = Vec 0 0
forceOn (Branch bb (p,_,m) a b c d) o
  | (width bb * height bb)/((distance' p (pos o))^2) < 0.5 = forceBetween (pos o) (mass o) p m
  | otherwise = (forceOn a o) `add` (forceOn b o) `add` (forceOn c o) `add` (forceOn d o)

forceBetween :: Vec -> Double -> Vec -> Double -> Vec
forceBetween p1 m1 p2 m2 = scale dir force
  where force = (gamma * m1 * m2) / ((distance' p1 p2) ^2)
        dir = unitify d
        d = diff p1 p2

updateObj :: Tree Object -> Object -> Object
updateObj tree obj = obj { speed = newSpeed
                         , pos = pos obj `add` newSpeed
                         }
  where force = forceOn tree obj
        newSpeed = speed obj `add` (scale force (1 / mass obj))

updateWorld :: [Object] -> [Object]
updateWorld objs = map (updateObj tree) os
  where initTree = buildTree $ objs
        os = collide initTree objs
        tree = buildTree os

updateWorldNoCol :: [Object] -> [Object]
updateWorldNoCol objs = map (updateObj tree) objs
  where tree = buildTree objs

{--
collide :: Tree Object -> Tree Object
collide n@(Empty _) = n
collide n@(Leaf bb o) = n
collide branch@(Branch bb (p,s,m) a b c d)
  | (width bb) * (height bb) < (sum $ map mass (treeLeafs branch))
  = Leaf bb (Object { pos=p, mass=m, speed=sumSpeeds (firstLevelObjects branch) })
  | otherwise = Branch bb (p,s,m) (collide a) (collide b) (collide c) (collide d)
--}

collide :: Tree Object -> [Object] -> [Object]
collide _ [] = []
collide tree (o:os) 
  | cand <- col o tree
  , not . null $ cand
  = let newObj = mergeObjects cand
        newOs = os \\ cand
        newTree = buildTree newOs
    in newObj : collide newTree newOs

-- returns the
col ::  Object -> Tree Object -> [Object]
col o (Empty _) = []
col o (Leaf bb a)
  | distance o a < (calcR o + calcR a) * (3/4) = [a]
  | otherwise = []
col o (Branch bb (p,s,m) a b c d) = concat $ map (ifThenIf (objIsctNode o) (col o)) [a,b,c,d]

ifThenIf :: (a -> Bool) -> (a -> [b]) -> a -> [b]
ifThenIf p f a
  | p a = f a
  | otherwise = []

objIsctNode o (Empty bb) = objIsctBB bb o
objIsctNode o (Leaf bb _) = objIsctBB bb o
objIsctNode o (Branch bb _ _ _ _ _) = objIsctBB bb o

objIsctBB :: BB -> Object -> Bool
objIsctBB bb@(nw, se) o
  | x - rad < l = True
  | x + rad > r = True
  | y - rad < u = True
  | y + rad > d = True
  | otherwise = pointInsideBB bb (pos o)
  where (Vec l u) = nw
        (Vec r d) = se
        (Vec x y) = pos o
        rad = calcR o

pointInsideBB :: BB -> Vec -> Bool
pointInsideBB (nw, se) (Vec x y)
  | l < x = True
  | x < r = True
  | u < y = True
  | y < d = True
  | otherwise = False
  where (Vec l u) = nw
        (Vec r d) = se

-- | the objects in xs that a collide with
checkCollide xs a = [ b | b <- xs , distance a b < calcR a + calcR b]

treeLeafs :: Tree a -> [a]
treeLeafs (Empty _) = []
treeLeafs (Leaf _ o) = [o]
treeLeafs (Branch _ _ a b c d) = (treeLeafs a) ++ (treeLeafs b) ++ (treeLeafs c) ++ (treeLeafs d)

-- the minimal are that maximum four bodies can be in
collisionThreshold = 100

-- | calculates the net momentum of a group of Objects and by it finds the 'sum' of their speeds
sumSpeeds :: [Object] -> Vec
sumSpeeds os = scale p (1/totMass)
  where p = vsum $ map (\x -> scale (speed x) (mass x)) os
        totMass = sum $ map mass os

mergeObjects :: [Object] -> Object
mergeObjects os = Object { pos = p, speed = spd, mass = m }
  where (p, spd, m) = newCm os

width :: BB -> Double
width (nw, se) = vx se - vx nw

height :: BB -> Double
height (nw, se) = vy se - vy nw

middle :: BB -> Vec
middle bb@(nw, se) = Vec x y
  where x = (vx nw) + (width bb / 2)
        y = (vy nw) + (height bb / 2)

distanceDivider = 10

distance a b = distance' (pos a) (pos b)

distance' a b = (sqrt $ (vx diff)^2 + (vy diff)^2) / distanceDivider
  where diff = a `sub` b

calcR :: Object -> Double
calcR o = (sqrt ((mass o) / pi)) / distanceDivider

insertIn :: Tree Object -> Object -> Tree Object
insertIn (Empty bb) o = Leaf bb o
insertIn t@(Branch bb cm a b c d) o
  | quad bb o == 1 = Branch bb (newCm (o : obs t)) (insertIn a o) b c d
  | quad bb o == 2 = Branch bb (newCm (o : obs t)) a (insertIn b o) c d
  | quad bb o == 3 = Branch bb (newCm (o : obs t)) a b (insertIn c o) d
  | otherwise      = Branch bb (newCm (o : obs t)) a b c (insertIn d o)
insertIn t@(Leaf bb a) o = insertIn (insertIn (Branch bb (Vec 0 0, Vec 0 0,0) (Empty (firstQuad bb)) (Empty (secondQuad bb)) (Empty (thirdQuad bb)) (Empty (fourthQuad bb))) a) o

newCm :: [Object] -> (Vec, Vec, Double)
newCm list = ( Vec (sum (zipWith (\a b -> (vx a) * b) poses masses)/smass) (sum (zipWith (\a b -> (vy a) * b) poses masses)/smass)
             , sumSpeeds list
             , smass
             )
  where masses = map mass list
        poses = map pos list
        smass = sum masses
