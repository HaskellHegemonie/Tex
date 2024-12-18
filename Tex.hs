{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FunctionalDependencies #-}
module Tex
  ( module TexClasses
  , module Tex
  , module GHC.OverloadedLabels
           )where
import TexClasses
import Text.Printf
import GHC.OverloadedLabels
import GHC.TypeLits
import Data.Proxy

data LTX = Sum LTX LTX
         | Negate LTX
         | Mult LTX LTX
         | Abs LTX
         | Signum LTX
         | Recip LTX
         | Exp LTX
         | ExpFun LTX
         | Power LTX LTX
         | Ln LTX
         | LogBase LTX LTX
         | Sin LTX
         | Cos LTX
         | Tan LTX
         | Asin LTX
         | Acos LTX
         | Atan LTX
         | Sinh LTX
         | Cosh LTX
         | Asinh LTX
         | Acosh LTX
         | Atanh LTX
         | PlusMinus LTX LTX
         | Special String
         | Subscript LTX LTX
         | Supscript LTX LTX
         | Laminate LTX LTX -- TODO: Eliminate
         | C String
         | Variable String
         | LTX :+ LTX

infixl 6 :+

instance KnownSymbol sym => IsLabel sym LTX where
  fromLabel = Variable $ symbolVal (Proxy @sym)

data Paren = S | M | P
  -- x@(Mult _ _) -> printf "\\left(%s\\right)" $ show x

paren M = \case
  x@(Sum _ _) -> printf "\\left(%s\\right)" $ show x
  x -> show x

paren P = \case
  x@(Sum _ _) -> printf "\\left(%s\\right)" $ show x
  x@(Mult _ _) -> printf "\\left(%s\\right)" $ show x
  x -> show x

paren _ = \case
  Special s -> s
  C x -> x
  Variable v -> v
  
instance Show LTX where
  show  = \case
    Sum x (Negate y) -> printf "%s - %s" (show x) (show y)
    Sum x y -> printf "%s + %s" (show x) (show y)
    Negate x -> printf "- %s" (show x)
    Mult x (Recip y) -> printf "\\frac{%s}{%s}" (show x) (show y)
    Mult x y -> printf "%s \\cdot %s" (paren M x) (paren M y)
    Recip x -> printf "\\frac{1}{%s}" (show x)
    Abs x -> printf "\\left| %s \\right|" (show x)
    Signum x -> undefined
    Exp x -> printf "{\\rm e}^{%s}" (show x)
    ExpFun x -> printf "\\exp\\left(%s\\right)" (show x)
    Power x (Recip (C "2")) -> printf "\\sqrt{%s}" (show x)
    Power x (Recip y) -> printf "\\sqrt[%s]{%s}" (show y) (show x)
    Power x y -> printf "%s^{%s}" (paren P x) (show y)
    Ln x -> printf "\\ln\\left(%s\\right)" (show x)
    LogBase x y -> printf "\\log_{%s}\\left(%s\\right)" (show x) (show y)
    Sin x -> printf "\\sin\\left(%s\\right)" (show x)
    Cos x -> printf "\\cos\\left(%s\\right)" (show x)
    Tan x -> printf "\\tan\\left(%s\\right)" (show x)
    Asin x -> printf "\\sin^{-1}\\left(%s\\right)" (show x)
    Acos x -> printf "\\cos^{-1}\\left(%s\\right)" (show x)
    Atan x -> printf "\\tan^{-1}\\left(%s\\right)" (show x)
    Sinh x -> printf "\\sinh\\left(%s\\right)" (show x)
    Cosh x -> printf "\\cosh\\left(%s\\right)" (show x)
    Asinh x -> printf "\\sinh^{-1}\\left(%s\\right)" (show x)
    Acosh x -> printf "\\cosh^{-1}\\left(%s\\right)" (show x)
    Atanh x -> printf "\\tanh^{-1}\\left(%s\\right)" (show x)
    PlusMinus x y -> printf "%s \\pm %s" (show x) (show y)
    Subscript x y -> printf "%s_{%s}" (show x) (show y)
    Supscript x y -> printf "%s^{%s}" (show x) (show y)
    Laminate x y -> printf "%s %s" (show x) (show y)
    Special x -> x
    C x -> x
    Variable x -> x
    (C "0") :+ ib -> printf "{\\rm i}\\cdot%s" (show ib)
    a :+ ib -> printf "%s + {\\rm i}\\cdot%s" (show a) (show ib)

instance Num LTX where
  (+) = Sum
  negate = Negate
  (*) = Mult
  abs = Abs
  signum = Signum
  fromInteger x = C $ show x

instance Fractional LTX where
  recip = Recip
  fromRational x = C $ show x

instance Floating LTX where
  pi = Special "\\pi"
  exp = Exp
  (**) = Power
  log = Ln
  logBase = LogBase
  sin = Sin
  cos = Cos
  asin = Asin
  acos = Acos
  atan = Atan
  sinh = Sinh
  cosh = Cosh
  asinh = Asinh
  acosh = Acosh
  atanh = Atanh

n :: Num a => a -> a
n = negate

r :: Fractional a => a -> a
r = recip

ln :: Floating a => a -> a
ln = log

instance PM LTX LTX where
  (±) = PlusMinus

instance Tex LTX where
  (↑) = Supscript
  (↓) = Subscript
  (⍪) = Laminate
