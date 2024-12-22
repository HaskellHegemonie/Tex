{-# LANGUAGE DataKinds #-}
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
import Data.List
import Text.Printf
import GHC.OverloadedLabels
import GHC.TypeLits
import Data.Proxy

instance KnownSymbol s => IsLabel s String where
  fromLabel = symbolVal (Proxy @s)

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
         | Tanh LTX
         | Asinh LTX
         | Acosh LTX
         | Atanh LTX
         | PlusMinus LTX LTX
         | Subscript LTX LTX
         | Supscript LTX LTX
         | Laminate LTX LTX -- TODO: Eliminate
         | Csc LTX
         | Sec LTX
         | Cot LTX
         | Acsc LTX
         | Asec LTX
         | Acot LTX
         | Lim LTX (LTX -> LTX) LTX
         | Derivative (LTX -> LTX) LTX
         | Sigma LTX LTX (LTX -> LTX) LTX
         | APi LTX LTX (LTX -> LTX) LTX
         | IntegralBound LTX LTX (LTX -> LTX) LTX
         | Integral (LTX -> LTX) LTX
         | Special String
         | C String
         | Variable String
         | FunDef String [LTX] LTX
         | FunApp String [LTX]
         | LTX :+ LTX

infixl 6 :+

instance KnownSymbol sym => IsLabel sym LTX where
  fromLabel = Variable $ symbolVal (Proxy @sym)

data Paren = S | M | P
  -- x@(Mult _ _) -> printf "\\left(%s\\right)" $ show x

paren M = \case
  x@(Sum _ _) -> printf "\\left(%s\\right)" $ show x
  x@(Negate _) -> printf "\\left(%s\\right)" $ show x
  x -> show x

paren P = \case
  Special s -> s
  C x -> x
  Variable v -> v
  x -> printf "\\left(%s\\right)" $ show x

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
    Tanh x -> printf "\\tanh\\left(%s\\right)" (show x)
    Asinh x -> printf "\\sinh^{-1}\\left(%s\\right)" (show x)
    Acosh x -> printf "\\cosh^{-1}\\left(%s\\right)" (show x)
    Atanh x -> printf "\\tanh^{-1}\\left(%s\\right)" (show x)
    PlusMinus x y -> printf "%s \\pm %s" (show x) (show y)
    Subscript x y -> printf "%s_{%s}" (show x) (show y)
    Supscript x y -> printf "%s^{%s}" (show x) (show y)
    Laminate x y -> printf "%s %s" (show x) (show y)
    Csc x -> printf "\\csc\\left(%s\\right)" (show x) 
    Sec x -> printf "\\sec\\left(%s\\right)" (show x)
    Cot x -> printf "\\cot\\left(%s\\right)" (show x)
    Acsc x -> printf "\\csc^{-1}\\left(%s\\right)" (show x)
    Asec x -> printf "\\sec^{-1}\\left(%s\\right)" (show x)
    Acot x -> printf "\\cot^{-1}\\left(%s\\right)" (show x)
    Lim to fun x -> printf "\\lim_{%s \\rightarrow %s} %s" (show x) (show to) (show $ fun x)
    Derivative f x -> printf "\\frac{\\rm d}{{\\rm d}%s} %s" (show x) (show $ f x)
    Sigma down up fun var -> printf "\\sum_{%s}^{%s} %s" (show down) (show up) (show $ fun var)
    APi down up fun var -> printf "\\prod_{%s}^{%s} %s" (show down) (show up) (show $ fun var)
    Integral f x -> printf "\\int %s d%s" (show $ f x) (show x)
    IntegralBound down up fun var ->  printf "\\int_{%s}^{%s} %s d%s" (show down) (show up) (show $ fun var) (show var)
    Special x -> x
    C x -> x
    Variable x -> if length x /= 1 then printf "\\mathrm{%s}" x else x
    FunDef name args def -> printf "%s(%s) = %s" name (intercalate "," $ map show args) (show def)
    FunApp name args -> printf "%s(%s)" name (intercalate "," $ map show args)
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
  tan = Tan
  asin = Asin
  acos = Acos
  atan = Atan
  sinh = Sinh
  cosh = Cosh
  tanh = Tanh
  asinh = Asinh
  acosh = Acosh
  atanh = Atanh

instance Enum LTX where
  toEnum x = C $ show x
  fromEnum = undefined

instance SeriesHelper LTX where
  combineLim = const
  combineDerive = const
  combineSigma = const
  combinePi = const

instance Series LTX where
  lim to f g = Lim to f (g to)
  derive = Derivative
  sigma a b f g = Sigma a b f (g b)
  aPi a b f g = APi a b f (g b)
  intBound a b f g = IntegralBound a b f (g b)
  int = Integral


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

instance Trig LTX where
  csc = Csc
  sec = Sec
  cot = Cot

  acsc = Acsc
  asec = Asec
  acot = Acot

generate :: [LTX] -> String
generate = concatMap (\x -> printf "\\[ %s \\]\n" $ show x)
generate_ = putStr . generate
