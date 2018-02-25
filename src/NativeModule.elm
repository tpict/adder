-- Thanks to
-- https://github.com/GlenDC/trixel/blob/refactor/src/Trixel/Glue/Random.elm

module NativeModule exposing (randomInt)
import Native.NativeModule

randomFloat : Float -> Float -> Float
randomFloat =
  Native.GlueRandom.randomFloat


randomInt : Int -> Int -> Int
randomInt =
  Native.GlueRandom.randomInt
