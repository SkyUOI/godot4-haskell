{-# LANGUAGE PatternSynonyms #-}

module Godot.Type.Vector2
  ( Vector2
  , Axis(..)
  , zero
  , one
  , inf
  , left
  , right
  , up
  , down
  , angle
  , angleTo
  , angleToPoint
  , pattern Vector2
  , aspect
  ) where

import qualified Linear.V2 as V

type Vector2 = V.V2 Float

pattern Vector2 :: Float -> Float -> Vector2
pattern Vector2 x y <- V.V2 x y
  where Vector2 x y = V.V2 x y

-- vector2 (from vector2i)

-- Corresponding to AXIS_X and AXIS_Y in godot
data Axis
  = AxisX
  | AxisY
  deriving (Show, Eq)

-- Constants corresponding ZERO, ONE, INF, LEFT, RIGHT, UP, DOWN in godot
zero, one, inf, left, right, up, down :: Vector2
zero = Vector2 0 0

one = Vector2 1 1

inf = Vector2 (1 / 0) (1 / 0)

left = Vector2 (-1) 0

right = Vector2 1 0

up = Vector2 0 (-1)

down = Vector2 0 1

angle :: Vector2 -> Float
angle = V.unangle

angleTo :: Vector2 -> Vector2 -> Float
angleTo x y = angle y - angle x

angleToPoint :: Vector2 -> Vector2 -> Float
angleToPoint x y = angle (y - x)

aspect :: Vector2 -> Float
aspect (Vector2 x y) = y / x
