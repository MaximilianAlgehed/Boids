module Example where

import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.Rasterific as B
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

bt :: [(Double, Double)] -> BoidTransform
bt avd = blend 0.7 remain $
   (foldl (<+>)
     (   align    `within` 3
     <+> cohesion  `upto` 10
     <+> avoidance `within` 2 `upto` 20
     ) (map (avoid . V) avd)
   )`upto` 3

fourFlock :: Flock
fourFlock =
  [ boid (-1, -1) (0,1) 
  , boid (1, 1)   (0,1)
  , boid (-1, 1)  (0,1)
  , boid (1, -1)  (0,1)
  ]

bt' :: BoidTransform
bt' = align <+> cohesion

makeBackground :: [(Double, Double)] -> D.Diagram B.B
makeBackground = foldl mappend mempty . map makeCircle
  where
    makeCircle :: (Double, Double) -> D.Diagram B.B
    makeCircle p = D.circle 0.1 D.# D.translate (D.r2 p) D.# D.fc D.red D.# D.lw D.none

points = [(3, 0), (0, 0), (2, 2), (-10, 0)]

main :: IO ()
main = makeGifWithBackground (makeBackground points) 500 50 fourFlock (bt points)
