module Main where

import CalculatorRegisterAST (CalcExprAST (..), CalcStmtAST (SetReg), Register (..), calculatorRegisterAST1, calculatorRegisterAST2, calculatorRegisterAST3, calculatorRegisterAST4, calculatorSetRegisterAST1, calculatorSetRegisterAST2, calculatorSetRegisterAST3, calculatorSetRegisterAST4)
import CalculatorRegisterStore (Store, getstore, registerstore, setstore)

getregisterindex :: Register -> Integer
getregisterindex Reg0 = 0
getregisterindex Reg1 = 1
getregisterindex Reg2 = 2
getregisterindex Reg3 = 3
getregisterindex Reg4 = 4
getregisterindex Reg5 = 5
getregisterindex Reg6 = 6
getregisterindex Reg7 = 7
getregisterindex Reg8 = 8
getregisterindex Reg9 = 9

eval :: CalcExprAST -> Store -> Integer
eval (Lit i) s = i
eval (Add l r) s = eval l s + eval r s
eval (Mult l r) s = eval l s * eval r s
eval (Sub l r) s = eval l s - eval r s
eval (Neg i) s = - eval i s
eval (Reg r) s = getstore s (getregisterindex r)

exec :: CalcStmtAST -> Store -> Store
exec (SetReg reg ast) s = setstore (getregisterindex reg) (eval ast s) s

main :: IO ()
main = do
  let store1 = exec calculatorSetRegisterAST1 registerstore
  let store2 = exec calculatorSetRegisterAST2 store1
  let store3 = exec calculatorSetRegisterAST3 store2
  let store4 = exec calculatorSetRegisterAST4 store3
  print store4
