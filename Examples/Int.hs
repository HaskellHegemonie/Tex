{-# LANGUAGE OverloadedLabels #-}
import Tex
import Data.Function
import Text.Printf

-- z = generate_
--  [ #x & \x -> 7 * r (x ** 4)
--  , #x & \x -> 7 * x ** n 4
--  , #x & \x -> 7 * r 3 * x ** n 3
--  ]


-- o = generate_
--   [ #x & \x -> (x ** 1) ** r 3
--   , #x & \x -> x ** (1 * r 3)
--   , #x & \x -> r (1 * r 3 + 1) * x ** (1 * r 3 + 1)
--   , #x & \x -> r (4 * r 3) * x ** (4 * r 3)
--   , #x & \x -> 3 * r 4 * x ** (4 * r 3)
--   ]


-- trig = generate_
--   [ #x & \x -> cos x
--   , #x & \x -> sin x
--   , #x & \x -> n (cos x)
--   , #x & \x -> n (sin x)
--   , #x & \x -> cos x
--   , #x & \x -> tan x
--   , #x & \x -> 8 * cos x + 3 * sin x
--   , #x & \x -> 8 * sin x + 3 * n (sin x)
--   ]

-- expFun = generate_
--   [ #x & \x -> exp (4 * x)
--   , #x & \x -> exp (4 * x) * r 4
--   , #x & \x -> exp (7 * x + n 3)
--   , #x & \x -> exp (7 * x + n 3) * r 7
--   , #x & \x -> exp (x * 2)
--   , #x & \x -> exp x ** 2
--   , #x & \x -> r 3 * exp x ** 3
--   ]

-- test = generate_
--   [ sec #x
--   , r (cos #x)
--   , asec #x
--   , acos (r #x)
--   ]


c = generate_ [f #x, i #x, fF1 #c #x]
  where
    f x = 4 * x ** 2
    i = intBound_ 0 1 f
    fF1 c x = 4 * r 3 * x ** 3 + c



a = generate_ [ ]
  where
    p a = a ** a
