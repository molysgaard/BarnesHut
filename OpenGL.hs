module Main where

import BarnesHut as O

import Graphics.UI.GLUT as G
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Control.Monad (unless,when,forM_)
import Data.IORef (IORef, newIORef)
import Data.List.Split (chunk)

import System.Random

delta :: Int
delta = 25

data State = State {
      world :: IORef [O.Object]
}

makeState :: IO State
makeState = do
  init <- O.randPlanets
  let sun = Object {pos = Vec 150 150, speed = Vec 0 0, mass = 1500}
  p <- newIORef (sun:init)
  return $ State p

-- TODO color
drawObject :: O.Object -> IO ()
drawObject o = preservingMatrix $ do
                 translate (Vector3 (realToFrac x) (realToFrac y) 0.0 :: Vector3 GLfloat)
                 renderObject Solid $ Sphere' radius 6 2
    where
      (Vec x1 y1) = O.pos o
      x = (x1 - 150) * 2
      y = (y1 - 150) * 2
      radius = realToFrac (calcR o * distanceDivider * 2)

colorByMass :: Double -> Color4 Double
colorByMass m = Color4 r g b 1 where
    b = min 255 (20.0 * m) / 255.0
    r = min 100 (255.0 - b) / 255.0
    g = 128.0
    
sizeByMass :: Double -> Double
sizeByMass = (+) 3.0

displayFunc :: State -> DisplayCallback
displayFunc state = do
  clear [ColorBuffer,DepthBuffer]
  materialAmbient Front $= Color4 1 0 0 1
  materialDiffuse Front $= Color4 0 1 0 1
  materialSpecular Front $= Color4 1 1 1 1
  materialShininess Front $= 1000
  s <- G.get (world state)
  mapM_ drawObject s
  swapBuffers

initGraphics :: IO ()
initGraphics = do
  depthFunc $= Just Less
  clearDepth $= 100
  matrixMode $= Modelview 0
  loadIdentity
  lighting $= Enabled
  light (Light 0) $= Enabled
  G.position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= Color4 1 1 1 1
  diffuse (Light 0) $= Color4 1 1 1 1
  specular (Light 0) $= Color4 1 1 1 1
  matrixMode $= Projection
  loadIdentity
  ortho (-500) 500 (-500) 500 200 (-200)

timerCallback :: State -> TimerCallback
timerCallback state = do
  world state $~ updateWorld
  postRedisplay Nothing
  addTimerCallback delta (timerCallback state)

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  initialWindowSize $= Size 512 512
  initialWindowPosition $= G.Position 0 0
  _ <- createWindow "Orbit in Haskell"
  _ <- initGraphics

  state <- makeState

  displayCallback $= displayFunc state
  addTimerCallback delta (timerCallback state)

  mainLoop
