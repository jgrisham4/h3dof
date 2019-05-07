module Interp
() where

import           Data.Vector as V

interp :: (RealFrac a) => Vector a -> Vector a -> a -> a
interp xv yv x
  | x <= V.head xv = V.head yv
  | x >= V.last xv = V.last yv
  | otherwise = m * x + b
    where
      i = V.last $ V.findIndices (<x) xv
      x1 = xv ! i
      x2 = xv ! (i+1)
      y1 = yv ! i
      y2 = yv ! (i+1)
      m = (y2 - y1) / (x2 - x1)
      b = y1 - m * x1

