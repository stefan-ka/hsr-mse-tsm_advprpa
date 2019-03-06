module ProgExercises.HS_2019_ProgExer01Prob_V01 where

-- Develop some functions to work with two-dimensional vectors,
-- using pairs, including pair patterns.

-- a type for two-dimensional vectors
type Vec = (Double, Double)

-- the zero vector
zeroVec :: Vec
zeroVec = (0, 0)

-- some example vectors
a, b, c, d :: Vec
a = (3, 0)
b = (0, 4)
c = (sqrt2, sqrt2)
      where sqrt2 = sqrt 2
d = (3, 4)

-- lengthVec computes the length of a vector.
exaLengthVec =
  lengthVec a == 3 && lengthVec c == 2

lengthVec :: Vec -> Double
lengthVec (x, y) = sqrt (x^2 + y^2)

-- negVec negates a vector.
exaNegVec =
  negVec d == (-3, -4)

negVec :: Vec -> Vec
negVec (x, y)  = (-x, -y)

-- negVecCurry negates a vector, but uses currying.
-- Note: this is a bad use of currying, since
-- the two components of a vector belong together.
exaNegVecCurry =
  negVecCurry (fst d) (snd d) == (-3, -4)

negVecCurry :: Double -> Double -> Vec
negVecCurry x y  = (-x, -y)

-- addVec adds two vectors.
exaAddVec =
  a `addVec` b == d

addVec :: Vec -> Vec -> Vec
addVec (x1, y1) (x2, y2)  = (x1 + x2, y1 + y2)

-- subVec subtracts two vectors.
exaSubVec =
  a `subVec` b == (3, -4)
-- Implement this function using negVec and addVec.

subVec :: Vec -> Vec -> Vec
subVec v1 v2 = v1 `addVec` negVec v2

-- subVecCurry subtracts two vectors.
-- Implement this function using negVecCurry and addVec.
-- Note: this example clearly demonstrates the bad use
-- of currying in negVecCurry.
-- The two components of a vector simply belong together.
exaSubVecCurry =
  a `subVecCurry` b == (3, -4)

subVecCurry :: Vec -> Vec -> Vec
subVecCurry v1 (x2, y2)  = addVec v1 (negVecCurry x2 y2)

-- distance computes the distance between two vectors.
exaDistance =
  distance a d == 4
-- Implement this function using subVec and lengthVec.

distance :: Vec -> Vec -> Double
distance v1 v2 = lengthVec (v1 `subVec` v2)

-- Scales a vector with a factor.
exaScaleVec =
  scaleVec d 3 == (9, 12)

scaleVec :: Vec -> Double -> Vec
scaleVec (x, y) s = (s * x, s * y)

