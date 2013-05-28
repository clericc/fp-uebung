{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
 
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
 
-- import Data.Maybe (fromMaybe)
 
import Control.Monad (liftM2)
import Control.Monad.Error 
 
-- ----------------------------------------
-- syntactic domains
 
data Expr  = Const  Int
           | Binary BinOp Expr Expr
             deriving (Show)
 
data BinOp = Add | Sub | Mul | Div | Mod | Nil
             deriving (Eq, Show)
 
-- ----------------------------------------
-- semantic domains
 
data Result a = Error String
              | Value      a

-- ----------------------------------------
-- class instances

instance Monad Result where
  return          = Value
  Error msg >>= _ = Error msg
  Value x >>= g   = g x

instance MonadError String Result where
  throwError msg = Error msg
  catchError (Value a) _ = Value a
  catchError (Error msg) f = f msg
  
instance Show a => Show (Result a) where
  show (Error msg) = "Error: " ++ msg
  show (Value   x) = "Value: " ++ show x

 
-- ----------------------------------------
-- the meaning of an expression
 
eval :: Expr -> Result Int
eval (Const i) = return i 
eval (Binary op l r) = lookupMft op >>= (\f -> f (eval l) (eval r))
 
-- ----------------------------------------
-- the meaning of binary operators
 
type MF = Result Int -> Result Int -> Result Int
 
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
 
e1 = eval $ Binary Mul (Binary Add (Const 4)
                                   (Const 2))
                       (Const 7)
e2 = eval $ (Binary Nil (Const 1) (Const 1))
e3 = eval $ Binary Mod (Const 1) (Const 0)
e4 = eval $ Binary Nil (Const 1) (Const 2)

