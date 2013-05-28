{-# OPTIONS
    -XTypeSynonymInstances
    -XMultiParamTypeClasses #-}
 
-- The following compiler options are neccessary
-- for the extension concerning the error handling
-- they are not required for this simple example
 
-- ----------------------------------------
--
-- Monadic version of simple expression
-- evaluator.
-- No extensions, same semantics as in Expr0
--
-- ----------------------------------------
 
module Expr1 where
 
import Data.Maybe
    ( fromMaybe )
 
import Control.Monad (liftM2)  
 
-- ----------------------------------------
-- syntactic domains
 
data Expr  = Const  Int
           | Binary BinOp Expr Expr
             deriving (Show)
 
data BinOp = Add | Sub | Mul | Div | Mod | Lol
             deriving (Eq, Show)
 
-- ----------------------------------------
-- semantic domains
 
data Result a = Error String
              | Value a
              

throwError = Error
                
 
-- ----------------------------------------
-- the identity monad
 
instance Monad Result where
  return x          = Value x
  (Error msg) >>= g = Error msg
  (Value   x) >>= g = g x
  
instance Show a => Show (Result a) where
  show (Error msg) = "Error: " ++ msg
  show (Value   x) = "Value: " ++ show x
 
-- ----------------------------------------
-- the meaning of an expression
 
eval :: Expr -> Result Int
eval (Const i)
  = return i
 
eval (Binary op l r)
  = lookupMft op >>= (\f -> f (eval l) (eval r))
 
-- ----------------------------------------
-- the meaning of binary operators
 
type MF = Result Int -> Result Int ->
          Result Int
 
lookupMft :: BinOp -> Result MF
lookupMft op
  = case lookup op mft of
    Nothing -> throwError "operation not implemented"
    Just mf -> return mf
 
mft :: [(BinOp, MF)]
mft
  = [ (Add, liftM2 (+))
    , (Sub, liftM2 (-))
    , (Mul, liftM2 (*))
    , (Div, liftM2 div)
    ]
 

 
-- ----------------------------------------
-- sample expressions
 
e1 = Binary Mul (Binary Add (Const 4)
                            (Const 2)
                )
                (Const 7)
e2 = Binary Div (Const 1) (Const 0)
e3 = Binary Mod (Const 1) (Const 0)
e4 = Binary Lol (Const 1) (Const 2)
 
v1 = eval e1
