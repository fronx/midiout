module Util where

import Prelude ((*), (/), (-), round)
import GHC.Base
import GHC.Real (Integral, fromIntegral)

div :: Int -> Int -> Int
div a b = round (toDouble a / toDouble b)

asPercentageOf :: Int -> Int -> Int
asPercentageOf length num = (100 * num) `div` length

toDouble :: Integral a => a -> Double
toDouble x = fromIntegral x
