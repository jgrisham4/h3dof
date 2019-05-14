import Environment
import SimUtils
import Graphics.Gnuplot.Simple

main = do
  let alt = (linspace 0 100000 100) :: [Double]
  let t = map temperature alt
  let p = map pressure alt
  print p
