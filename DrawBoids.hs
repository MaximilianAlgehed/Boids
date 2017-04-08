{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module DrawBoids where

import Diagrams.Prelude hiding (position)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.CmdLine hiding (interval)
import Diagrams.Size
import Diagrams.Core.Transform

import Boids as BDS

drawBoid :: Double -> Boid -> Diagram B
drawBoid scale boid = tri
  where
    ang    = (angle (velocity boid) - pi/2) @@ rad
    rot    = rotation ang

    pos    = apply (inv rot) (r2 (unVec (BDS.position boid)))
    trans  = translation pos

    tri    = triangle scale # transform (rot <> trans)

drawFlock :: Double -> Flock -> Diagram B
drawFlock scale boids = foldl (<>) mempty [ drawBoid scale boid | boid <- boids ]

step :: BoidTransform -> Double -> [Boid] -> [Boid]
step bt dt = applyBT bt . update dt

steps :: BoidTransform -> Double -> Int -> [Boid] -> [Boid]
steps bt dt n = foldl (.) id (replicate n (step bt dt)) 

animate :: BoidTransform -> Double -> Flock -> Active Flock
animate bt dt flock = steps bt dt <$> (floor <$> interval 0 100) <*> pure flock

bound :: Double -> Flock -> Double
bound sz = foldl max 0 . map (\b -> let (x, y) = unVec (position b) in sz ** 2 + max (abs x) (abs y))

gif :: Flock -> BoidTransform -> [(QDiagram Cairo V2 Double Any, Int)]
gif flock bt =
  let sz     = 1 
      flocks = [ runActive (animate bt 0.1 flock) (toTime t) | t <- [0, 1 .. 100] ]
      maxB   = maximum (bound sz <$> flocks)
      diags  = (\f ->
                (  square (2*(maxB + sz)) # opacity 0 # showOrigin
                <> drawFlock sz f
                ) # bg white
               ) <$> flocks
  in  [ (d, 5) | (d, t) <- zip diags [0, 1 .. 100] ]

makeGif :: Flock -> BoidTransform -> IO ()
makeGif flock bt = gifRender (DiagramOpts (Just 500) (Just 500) "output.gif", GifOpts False False Nothing) (gif flock bt)
