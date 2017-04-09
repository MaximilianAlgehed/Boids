{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Example where

import Boids
import DrawBoids

flock :: Flock
flock =
    [ boid (1, 1)    (0.3, 3)
    , boid (2, 2)    (1, 5)
    , boid (3, 1)    (2, 0.3)
    , boid (1, 0)    (0.2, 1)
    , boid (-2, -3)  (0.2, -1)
    , boid (1, 5)    (1, 2)
    , boid (2, 2)    (3, 1)
    , boid (1, 4)    (-2, -2)
    , boid (-2, -6)  (-2, 1)
    , boid (2, -4)   (1, 1)
    , boid (1, 1)    (3, -3)
    , boid (-3, -1)  (2, 5)
    , boid (2, -3)   (-3, -7)
    ]

bt :: BoidTransform
bt =  blend 0.7 (blend 0.9 remain (constant (-1)))
   $  align     `within` 3
  <+> cohesion  `upto` 1
  <+> avoidance `within` 2 `upto` 2

main :: IO ()
main = makeGif 500 100 flock bt
