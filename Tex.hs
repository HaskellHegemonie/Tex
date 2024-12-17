{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}
module Test where
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
         | Special String
         | C String
         | Variable String
         | LTX :+ LTX
       
instance KnownSymbol sym => IsLabel sym LTX where
  fromLabel = Variable $ symbolVal (Proxy @sym)
instance Show LTX where
  show  = \case
    Sum x (Negate y) -> printf "%s - %s" (show x) (show y)
    Sum x y -> printf "%s + %s" (show x) (show y)
    Negate x -> printf "- %s" (show x)
    Mult x (Recip y) -> printf "\\frac{%s}{%s}" (show x) (show y)
    Mult x y -> printf "%s \\cdot %s" (show x) (show y)
    Recip x -> printf "\\frac{1}{%s}" (show x)
    Abs x -> printf "\\left| %s \\right|" (show x)
    Signum x -> undefined
    Exp x -> printf "{\\rm e}^{%s}" (show x)
    ExpFun x -> printf "\\exp\\left(%s\\right)" (show x)
    Power x y -> printf "%s^{%s}" (show x) (show y)
    Ln x -> printf "\\ln\\left(%s\\right)" (show x)
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
    Special x -> x
    C x -> x
    Variable x -> x
    a :+ ib -> printf "%s + \\mathbf{ib}\\cdot%s" (show a) (show ib)
  
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
  

n = negate
r = recip
ln = log
