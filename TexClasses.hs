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

class Floating a => Trig a where
  csc :: a -> a
  sec :: a -> a
  cot :: a -> a

  acsc :: a -> a
  asec :: a -> a
  acot :: a -> a

instance Trig Double where
  csc = recip . sin
  sec = recip . cos
  cot = recip . tan

  acsc = asin . recip
  asec = acos . recip
  acot = atan . recip

instance Trig Float where
  csc = recip . sin
  sec = recip . cos
  cot = recip . tan

  acsc = asin . recip
  asec = acos . recip
  acot = atan . recip

infixl 9 ↑
infixl 9 ↓
