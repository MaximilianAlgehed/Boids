module DrawBoidsUtility where

import Diagrams.Prelude
import Diagrams.Backend.Rasterific

dots :: [(Double, Double)] -> Diagram B
dots = foldl mappend mempty . map makeCircle
  where
    makeCircle :: (Double, Double) -> Diagram B
    makeCircle p = circle 0.1 # translate (r2 p) # fc red # lw none

field :: [[(Double, Double)]] -> ((Double, Double) -> (Double, Double)) -> Diagram B
field points f = foldl mappend mempty (map arrow (concat points))
  where
    arrow :: (Double, Double) -> Diagram B
    arrow pt
      | f pt /= (0, 0) = arrowAt' (with & headLength .~ verySmall) (p2 pt) (r2 (f pt)) # fc grey # opacity 0.5
      | otherwise      = mempty

grid :: Double -> Double -> Double -> [[(Double, Double)]]
grid sx sy step = [[(x, y) | x <- [(-sx), -sx + step .. sx ]] | y <- [-sy, -sy + step .. sy ]]
