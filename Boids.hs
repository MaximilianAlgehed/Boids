module Boids where
import Data.List
import Control.DeepSeq

type DeltaTime = Double

newtype Vec = V { unVec :: (Double, Double) } deriving (Eq, Ord)

instance Show Vec where
  show (V p) = show p

instance Num Vec where
  (V (a, b)) + (V (x, y)) = V (a + x, b + y)
  negate (V (a, b)) = V (negate a, negate b)
  fromInteger a = scale (fromInteger a) $ V (1, 0)

mag :: Vec -> Double
mag (V (a, b)) = sqrt (a**2 + b**2)

scale :: Double -> Vec -> Vec
scale s (V (a, b)) = V (s * a, s * b)

norm :: Vec -> Vec
norm (V (0, 0)) = V (0, 0)
norm v = scale (recip (mag v)) v

angle :: Vec -> Double
angle (V (x, y)) = let ang = atan (y / x)
                   in if x < 0 then ang + pi else ang

data Boid = Boid { velocity :: Vec
                 , position :: Vec
                 } deriving (Show, Eq, Ord)

boid :: (Double, Double) -> (Double, Double) -> Boid
boid p v = Boid (V v) (V p)

posX :: Boid -> Double 
posX = fst . unVec . position

posY :: Boid -> Double
posY = snd . unVec . position

update :: DeltaTime -> [Boid] -> [Boid]
update dt bds = [ b { position = position b + scale dt (velocity b) } | b <- bds ]

type Flock = [Boid]

-- | A `BoidTransform` takes a boid,
-- a list of boids representing the entire
-- flock (apart from the boid under consideration)
-- and produces a velocity
type BoidTransform = Boid -> [Boid] -> Vec 

(<+>) :: BoidTransform -> BoidTransform -> BoidTransform
(f <+> g) b bs = (f b bs) + (g b bs)

infixl 3 <+>

applyBT :: BoidTransform -> [Boid] -> [Boid]
applyBT bt bds =
  let bds' = zip [0..] bds in
  [ b { velocity = bt b (snd <$> (filter (/=(idx, b)) bds'))} | (idx, b) <- bds' ]

remain :: BoidTransform
remain b _ = velocity b

averageBy :: (Boid -> Vec) -> BoidTransform
averageBy f b bds = scale (recip (genericLength bds + 1)) (sum [f boid | boid <- (b:bds)])

cohesion :: BoidTransform
cohesion b bds = averageBy position b bds - position b

align :: BoidTransform
align = averageBy velocity 

constant :: Vec -> BoidTransform
constant v _ _ = v

avoidance :: BoidTransform
avoidance b bs = sum [ let v = position b - position b' in
                        scale (recip ((mag v) + 1)) (norm v)
                     | b' <- bs ]

within :: BoidTransform -> Double -> BoidTransform
within transform r b bds =
  transform b
  (filter (\b' -> mag (position b' - position b) <= r) bds)

infixl 4 `within`

upto :: BoidTransform -> Double -> BoidTransform
upto bt lim b bds = let v = bt b bds in
                      if mag v > lim then
                        scale lim (norm v)
                      else
                        v

infixl 4 `upto`

blend :: Double -> BoidTransform -> BoidTransform -> BoidTransform
blend sf f g b bts = scale sf (f b bts) + scale (1 - sf) (g b bts)
