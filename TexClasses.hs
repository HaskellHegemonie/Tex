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
