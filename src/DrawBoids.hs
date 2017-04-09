{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module DrawBoids where

import Diagrams.Prelude hiding (position)
import Diagrams.Backend.Rasterific
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
step bt dt = update dt . applyBT bt

bound :: Double -> Flock -> Double
bound sz = maximum . map (\b -> let (x, y) = unVec (position b) in sz ** 2 + max (abs x) (abs y))

gif :: Rational -> Flock -> BoidTransform -> [QDiagram B V2 Double Any]
gif maxT flock bt =
  let sz = 1 
      flocks t f fs
        | t <= 0    = f:fs
        | otherwise = flocks (t - 1) (step bt 0.1 f) (f:fs)
        
      theFlocks = reverse (flocks maxT flock [])

      maxB   = maximum (bound sz <$> theFlocks)
      diags  = (\f ->
                (  square (2*(maxB + sz)) # opacity 0
                <> drawFlock sz f
                ) # bg white
               ) <$> theFlocks
  in diags 

makeGif :: Double -> Rational -> Flock -> BoidTransform -> IO ()
makeGif sz maxT flock bt =
  animatedGif "output.gif"
              (dims (r2 (sz, sz)))
              LoopingForever
              5
              (gif maxT flock bt)
