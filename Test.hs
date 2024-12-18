{-# LANGUAGE OverloadedLabels #-}
module Test where
import Tex

pythag a b = (a ** 2 + b ** 2) ** r 2
pythagGen = pythag #a #b :: LTX

quadr a b c = (n b ± (b ** 2 + n (4 * a * c)) ** r 2) * r (2 * a)
quadrGen = quadr #a #b #c :: LTX


pUnit θ = sin θ ** 2 + cos θ ** 2
pUnitGen = pUnit #t :: LTX -- wrong order of power for now

piTest = 3 * r 2 * pi :: LTX
piTest' = 3 * pi * r 2 :: LTX
piTest'' = pi * 3 * r 2 :: LTX
piTest''' = pi * (3 * r 2) :: LTX


distri x = (2 * x) * (1 + 5)
distriGen = distri #x :: LTX


powTest = 2 * 5 ** (pi * r 2 * 3) :: LTX
powTest' = (2 * 5) ** (pi * r 2 * 3) :: LTX

test = exp (0 :+ pi)


sigma k a c = #Σ ↓ 0 ↑ k ⍪ a ↓ k * c ↓ k -- Σ as a function?
sigmaGen = aoeu #k #a #c :: LTX
