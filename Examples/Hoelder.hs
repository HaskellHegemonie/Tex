{-# LANGUAGE OverloadedLabels #-}
import Tex

hoelderTex cn p x = (r cn * sigma_ 0 cn (\x -> x ** p) x) ** r p
hoel = hoelderTex #n #p #x :: LTX
hoelder texi p xs = (r cn * sigma_ 0 (cn + n 1) (\i -> xs !! (truncate i) ** p) texi ) ** r p
  where
    cn = fromIntegral $ length xs
hoelderNum = hoelder 0
arit, quad, geom, harm :: [Double] -> Double
arit = hoelderNum 1
quad = hoelderNum 2
geom = hoelderNum 1e-6
harm = hoelderNum (n 1)
