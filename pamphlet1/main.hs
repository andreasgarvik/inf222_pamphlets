module Main where

import CalculatorAST
  ( CalcExprAST (..),
    calculatorAST1,
    calculatorAST2,
  )

eval :: CalcExprAST -> Integer
eval (Lit i) = i
eval (Add l r) = eval l + eval r
eval (Mult l r) = eval l * eval r
eval (Sub l r) = eval l - eval r
eval (Neg i) = - eval i

main :: IO ()
main = do
  print (eval calculatorAST1)
  print (eval calculatorAST2)
