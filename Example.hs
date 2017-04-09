{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Example where

import Boids as BDS
import DrawBoids

flock :: [Boid]
flock = [ boid (1, 1) (0.3, 0.3)
        , boid (2, 2) (0.1, 0.5)
        , boid (3, 1) (0.5, 0.3)
        , boid (1, 0) (0.2, 1)
        , boid (-10, 10) (1, 3)
        ]

bt :: BoidTransform
bt = blend 0.3 remain (align <+> cohesion <+> (avoidance `within` 2 `upto` 0.5))

main :: IO ()
main = makeGif flock bt
