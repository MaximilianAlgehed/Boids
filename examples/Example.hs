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
bt =  blend 0.7 remain $
   (   align    `within` 3
   <+> cohesion  `upto` 10
   <+> avoidance `within` 2 `upto` 20
   <+> avoid origin 
   <+> avoid (V (-1, -1)) 
   <+> avoid (V (1, 1)) 
   <+> avoid (V (3, 0))
   ) `upto` 3

fourFlock :: Flock
fourFlock =
  [ boid (-1, -1) (0,0)  
  , boid (1, 1)   (0,0) 
  , boid (-1, 1)  (0,0) 
  , boid (1, -1)  (0,0) 
  ]

bt' :: BoidTransform
bt' = align <+> cohesion

main :: IO ()
main = makeGif 500 50 fourFlock avoidance
