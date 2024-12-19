{-# LANGUAGE OverloadedLabels #-}
import Tex

tex = generate_ [ Fun #f [#x] (f #x), i, Fun #F [#c, #x] (fF1 #c #x)]
  where
    f x = 8 * x ** 3
    i = int f #x
    fF1 c x = 2 * x ** 4 + c


tex0 = generate_ [Fun #f [#x] (f #x), i, Fun #F [#c, #x] (fF1 #c #x)]
  where
    f x = 3 * x ** 2 + 4 * x + 2
    i = int f #x
    -- fF1 = 3 * r 3 * x ** 3 + 4 * r 2 * x ** 2 + 2 * r 1 * x ** 1
    fF1 c x = x ** 3 + 2 * x ** 2 + 2 * x + c


a0 = generate_ [ Fun #f [#x] (f #x), i, Fun #F [#c, #x] (fF1 #c #x)]
  where
    f x = 5 * r 2 * (x + 1) ** 4
    i = intBound #x 0 3 f
    -- fF1 c x = 5 * r 2 * r 5 * (x + 1) ** 5 + c
    fF1 c x = r 2 * (x + 1) ** 5 + c

a1 = generate_ [Fun #f [#x] (f #x), i, Fun #F [#c, #x] (fF1 #c #x)]
  where
    -- f x = 2 * r (x ** 4)
    f x = 2 * x ** (n 4)
    i = int f #x
    fF1 c x = 2 * r (n 3) * x ** (n 3) + c
