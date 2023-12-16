-- | A port of Jonas Wagner's `simplex-noise` package.
module Performace.SimplexNoise where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST.Class (liftST)
import Data.Array (index)
import Data.Array.ST as STA
import Data.Int (floor, toNumber)
import Data.Int.Bits ((.&.))
import Data.Maybe (fromMaybe)
import Data.Number (sqrt)
import Data.Tuple.Nested ((/\))
import Effect.Random (randomInt)
import Effect.Unsafe (unsafePerformEffect)

f2 :: Number
f2 = 0.5 * (sqrt 3.0 - 1.0)

g2 :: Number
g2 = (3.0 - sqrt 3.0) / 6.0

f3 :: Number
f3 = 1.0 / 3.0

g3 :: Number
g3 = 1.0 / 6.0

f4 :: Number
f4 = (sqrt 5.0 - 1.0) / 4.0

g4 :: Number
g4 = (5.0 - sqrt 5.0) / 20.0

grad2 :: Array Number
grad2 = map toNumber
  [  1,  1
  , -1,  1
  ,  1, -1

  , -1, -1
  ,  1,  0
  , -1,  0

  ,  1,  0
  , -1,  0
  ,  0,  1

  ,  0, -1
  ,  0,  1
  ,  0, -1
  ]

grad3 :: Array Number
grad3 = map toNumber
  [  1,  1,  0
  , -1,  1,  0
  ,  1, -1,  0

  , -1, -1,  0
  ,  1,  0,  1
  , -1,  0,  1

  ,  1,  0, -1
  , -1,  0, -1
  ,  0,  1,  1

  ,  0, -1,  1
  ,  0,  1, -1
  ,  0, -1, -1
  ]

grad4 :: Array Number
grad4 = map toNumber
  [  0,  1, 1, 1,  0,  1,  1, -1,  0,  1, -1, 1,  0,  1, -1, -1
  ,  0, -1, 1, 1,  0, -1,  1, -1,  0, -1, -1, 1,  0, -1, -1, -1
  ,  1,  0, 1, 1,  1,  0,  1, -1,  1,  0, -1, 1,  1,  0, -1, -1
  , -1,  0, 1, 1, -1,  0,  1, -1, -1,  0, -1, 1, -1,  0, -1, -1
  ,  1,  1, 0, 1,  1,  1,  0, -1,  1, -1,  0, 1,  1, -1,  0, -1
  , -1,  1, 0, 1, -1,  1,  0, -1, -1, -1,  0, 1, -1, -1,  0, -1
  ,  1,  1, 1, 0,  1,  1, -1,  0,  1, -1,  1, 0,  1, -1, -1,  0
  , -1,  1, 1, 0, -1,  1, -1,  0, -1, -1,  1, 0, -1, -1, -1,  0
  ]

-- | Create a 2D noise function
noise2D :: Number -> Number -> Number
noise2D = \x y -> do
  let
    -- skew the input space to determine which simplex cell we're in
    s = (x + y) * f2  -- hairy factor fro 2D
    i = floor (x + s)
    j = floor (y + s)
    t = toNumber (i + j) * g2
    x0_ = toNumber i - t  -- unskew the cell origin back to (x,y) space
    y0_ = toNumber j - t
    x0 = x - x0_  -- the x,y distance form the cell origin
    y0 = y - y0_
    -- for the 2D case, the simplex shape is an equilateral triangle.
    -- determine which simplex we are in.
    -- x0 > y0 then the lower triangle, XY order: (0,0)->(1,0)->(1,1)
    -- otherwise the upper triangle, YX order: (0.0)->(0.1)->(1,1)
    i1 /\ j1 = if x0 > y0 then 1 /\ 0 else 0 /\ 1
    -- a step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
    -- a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
    -- c = (3-sqrt(s))/6
    x1 = x0 - toNumber i1 + g2  -- offset for middle corner in (x,y) unskewed coords
    y1 = y0 - toNumber j1 + g2
    x2 = x0 - 1.0 + 2.0 * g2  -- offset for last corner in (x,y) unskewed coords
    y2 = y0 - 1.0 + 2.0 * g2
    -- work out the hashed gradient indices of the three simplex corners
    ii = i .&. 255
    jj = j .&. 255

    -- n0, n1, n2 - noise contribution from the three corners
    -- calculate the contribution from the three corners
    t0 = 0.5 - x0 * x0 - y0 * y0
    n0 =
      if t0 >= 0.0 then do
        let
          gi0 = ii + (index perm jj # fromMaybe 0)
          g0x = index permGrad2x gi0 # fromMaybe 0.0
          g0y = index permGrad2y gi0 # fromMaybe 0.0
          t0' = t0 * t0
        t0' * t0' * (g0x * x0 + g0y * y0)
      else 0.0

    t1 = 0.5 - x1 * x1 - y1 * y1
    n1 =
      if t1 >= 0.0 then do
        let
          gi1 = ii + i1 + (jj + j1 # index perm # fromMaybe 0)
          g1x = index permGrad2x gi1 # fromMaybe 0.0
          g1y = index permGrad2y gi1 # fromMaybe 0.0
          t1' = t1 * t1
        t1' * t1' * (g1x * x1 + g1y * y1)
      else 0.0

    t2 = 0.5 - x2 * x2 - y2 * y2
    n2 =
      if t2 >= 0.0 then do
        let
          gi2 = ii + 1 + (jj + 1 # index perm # fromMaybe 0)
          g2x = index permGrad2x gi2 # fromMaybe 0.0
          g2y = index permGrad2y gi2 # fromMaybe 0.0
          t2' = t2 * t2
        t2' * t2' * (g2x * x2 + g2y * y2)
      else 0.0
  -- add contributions from each corner to get the final noise value.
  -- the result is scaled to return values in the interval [-1,1].
  70.0 * (n0 + n1 + n2)
  where
  perm = buildPermutationTable
  permGrad2x = map (\i -> i `mod` 12 * 2 # index grad2 # fromMaybe 0.0) perm
  permGrad2y = map (\i -> i `mod` 12 * 2 + 1 # index grad2 # fromMaybe 0.0) perm

buildPermutationTable :: Array Int
buildPermutationTable = unsafePerformEffect do
  res <- liftST $ STA.new
  let
    init 256 = pure $ Done unit
    init i = do
      _ <- liftST $ STA.push i res
      pure $ Loop (i + 1)

    permutate 255  = pure $ Done unit
    permutate i = do
      i' <- randomInt i 512
      v1 <- liftST $ STA.peek i res
      v2 <- liftST $ STA.peek i' res
      _ <- liftST $ STA.poke i (fromMaybe 0 v2) res
      _ <- liftST $ STA.poke i' (fromMaybe 0 v1) res
      pure $ Loop (i + 1)

    fill 512 = pure $ Done unit
    fill i = do
      v <- liftST $ STA.peek (i - 256) res
      _ <- liftST $ STA.push (fromMaybe 0 v) res
      pure $ Loop (i + 1)

  tailRecM init 0
  tailRecM permutate 0
  tailRecM fill 256
  liftST (STA.unsafeFreeze res)

