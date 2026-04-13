module Expr where

import Numeric.Natural (Natural)

data BinOpType = Add | Sub | Mul | Div | Pow
  deriving (Eq, Enum, Bounded)

instance Show BinOpType where
  show :: BinOpType -> String
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

applyBin :: (Integral a) => BinOpType -> (a -> a -> a)
applyBin Add = (+)
applyBin Sub = (-)
applyBin Mul = (*)
applyBin Div = div
applyBin Pow = (^)

data UnrOpType = Neg | Fct
  deriving (Eq, Enum, Bounded)

instance Show UnrOpType where
  show :: UnrOpType -> String
  show Neg = "-"
  show Fct = "!"

applyUnr :: (Integral a) => UnrOpType -> (a -> a)
applyUnr Neg = negate
applyUnr Fct = \n -> product [1 .. n]

data Expr a
  = Lit a
  | UnrOp UnrOpType (Expr a)
  | BinOp BinOpType (Expr a) (Expr a)
  deriving (Foldable)

eval :: (Integral a) => Expr a -> Maybe Integer
eval (Lit n) = Just $ toInteger n
eval (UnrOp opType e) = applyUnr opType <$> eval e
eval (BinOp opType e1 e2) = do
  lhs <- eval e1
  rhs <- eval e2
  if opType == Div && rhs == 0
    then Nothing
    else Just $ lhs `op` rhs
  where
    op = applyBin opType

type ExprNat = Expr Natural