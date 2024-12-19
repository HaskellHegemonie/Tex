{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module TexClasses where
class PM a out | a -> out where
  (±) :: a -> a -> out

infixl 6 ±

class Tex a where
  (↑) :: a -> a -> a
  (↓) :: a -> a -> a
  (⍪) :: a -> a -> a

infixl 9 ↑
infixl 9 ↓

instance {-# OVERLAPS #-} Tex a where
  x ↑ _ = x
  x ↓ _ = x
  a ⍪ b = undefined

class Floating a => Trig a where
  csc :: a -> a
  sec :: a -> a
  cot :: a -> a

  acsc :: a -> a
  asec :: a -> a
  acot :: a -> a

instance {-# OVERLAPS #-} Floating a => Trig a where
  csc = recip . sin
  sec = recip . cos
  cot = recip . tan

  acsc = asin . recip
  asec = acos . recip
  acot = atan . recip

class (Fractional a, Enum a) => Series a where
  sigma :: a -> a -> a -> (a -> a) -> a
  aPi :: a -> a -> a -> (a -> a) -> a
  intBound :: a -> a -> a -> (a -> a) -> a
  int :: (a -> a) -> a -> a
  
instance {-# OVERLAPS #-} (Fractional a, Enum a) => Series a where
  sigma var a b f = sum $ map f [a..b]
  aPi var a b f = product $ map f [a..b]
  intBound var a b f = sum $ map ((* c) . f) [a, a + c..b]
    where
      c = 1e-4
  int f b = intBound 0 0 b f

sigma_ = sigma 0
aPi_ = aPi 0 -- boundify?
