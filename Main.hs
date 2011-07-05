module Main where

-- Barnes & Hut orbital simulator by Morten Olsen Lysgaard
-- mortenlysgaard.com

import BarnesHut as O
import Text.Printf

type Polygon = [O.Vec]
type Color = (Int, Int, Int)

writePoint :: O.Vec -> String 
writePoint (O.Vec x y) = (show x)++","++(show y)++" "

writePolygon :: (Color,Polygon) -> String 
writePolygon ((r,g,b),p) = "<polygon points=\""++(concatMap writePoint p)++"\" style=\"fill:#cccccc;stroke:rgb("++(show r)++","++(show g)++","++(show b)++");stroke-width:2\"/>"

writeBB :: (Color,BB) -> String
writeBB (c, (Vec l u, Vec r d)) = writePolygon (c, [Vec l u, Vec r u, Vec r d, Vec l d, Vec l u])

writePlanet :: Object -> String
writePlanet o = "<circle cx=\"" ++ (show x) ++ "\" cy=\"" ++ (show y) ++ "\" r=\"" ++ (show r) ++ "\" stroke=\"black\" stroke-width=\"1\" fill=\"black\"/>"
  where (Vec x y) = pos o
        r = calcR o * distanceDivider

writePlanets :: [Object] -> String
writePlanets os = concatMap writePlanet os

writeBBs :: [BB] -> String 
writeBBs bb = concatMap writeBB (blue bb)

header = concat
 [ "<?xml version=\"1.0\" standalone=\"no\"?>"
 , " <!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
 , " \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
 , " <svg width=\"300px\" height=\"300px\" version=\"1.1\""
 , " xmlns=\"http://www.w3.org/2000/svg\">"]

footer = "</svg>"

colorize = zip.repeat

blue = colorize (0,0,255)

writeFrame :: Int -> [BB] -> [Object] -> IO ()
writeFrame frame bbs os = writeFile ("frame"++(printf "%04d" frame)++".svg") $ header ++ (writePlanets os) ++ footer
--writeFrame frame bbs os = writeFile ("frame"++(printf "%04d" frame)++".svg") $ header ++ (writeBBs bbs) ++ (writePlanets os) ++ footer

treeToBbs (Empty _) = []
treeToBbs (Leaf bb _) = [bb]
treeToBbs (Branch _ _ os) = concat $ map treeToBbs os

main = do
       init <- O.randPlanets
       let sun = Object {pos = Vec 150 150, speed = Vec 0 0, mass = 1500}
       loop 0 (sun:init)

loop 500 _ = return ()
loop i objs = do
       let tree = O.buildTree objs
           newObj = O.updateWorld objs
       writeFrame i (treeToBbs tree) objs
       loop (i + 1) newObj
