{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module TexClasses where
import Data.Function
import Data.Complex qualified as Complex

n :: Num a => a -> a
n = negate

r :: Fractional a => a -> a
r = recip

ln :: Floating a => a -> a
ln = log


class Imaginary a b | b -> a where
  (+:) :: a -> a -> b

infixl 6 +:
instance Imaginary a (Complex.Complex a) where
  a +: b = a Complex.:+ b

i x = 0 +: x

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
  lim :: a -> (a -> a) -> (a -> a) -> a
  derive :: (a -> a) -> a -> a
  sigma :: a -> a -> (a -> a) -> (a -> a) -> a
  aPi :: a -> a -> (a -> a) -> (a -> a) -> a
  intBound :: a -> a -> (a -> a) -> (a -> a) -> a
  int :: (a -> a) -> a -> a
  
instance {-# OVERLAPS #-} (Fractional a, Enum a) => Series a where
  lim x f g = x + c & g & f
    where
      c = 1e-6
  derive f x = (f (x + c) + negate (f x)) * recip c
    where
      c = 1e-6
  sigma a b f g = sum $ map (f . g) [a..b]
  aPi a b f g = product $ map (f . g) [a..b]
  intBound a b f g = sum $ map ((* c) . f . g) [a, a + c..b]
    where
      c = 1e-4
  int f b = intBound 0 b f id

class SeriesHelper a where
  combineLim :: a -> a -> a
  combineDerive :: a -> a -> a
  combineSigma :: a -> a -> a
  combinePi :: a -> a -> a



instance {-# OVERLAPS #-} Num a => SeriesHelper a where
  combineLim = undefined
  combineDerive = undefined
  combineSigma = (+)
  combinePi = (*)


class (Enum a, Fractional a) => Helper a where
  fac :: a -> a
  binom :: a -> a -> a
  binom x y = fac x * recip (fac y * fac (x + negate y))
  {-# MINIMAL fac #-}

instance {-# OVERLAPS #-} (Enum a, Fractional a) => Helper a where
  fac x = product [1..x]

lim_ :: (Series a, SeriesHelper a) => a -> (a -> a) -> a -> a
lim_ a f x = lim a f (combineLim x)
derive_ :: (Series a, SeriesHelper a) => a -> (a -> a) -> a -> a
derive_ a f x = lim a f (combineLim x)
sigma_ :: (Series a, SeriesHelper a) =>  a -> a -> (a -> a) -> a -> a
sigma_ a b f = sigma a b f . combineSigma
aPi_ :: (Series a, SeriesHelper a) =>  a -> a -> (a -> a) -> a -> a
aPi_ a b f = aPi a b f . combinePi
intBound_ :: (Series a, SeriesHelper a) =>  a -> a -> (a -> a) -> a -> a
intBound_ a b f = intBound a b f . combineSigma
