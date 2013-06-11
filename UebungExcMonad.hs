{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
 
module Expr1 where
 
-- import Data.Maybe (fromMaybe)
 
import Control.Monad (liftM2)
import Control.Monad.Error
 
-- ----------------------------------------
-- syntactic domains
 
data Expr  = Const  Int
           | Binary BinOp Expr Expr
             deriving (Show)
 

data BinOp = Add | Sub | Mul | Div | Mod | PlusMinus
               deriving (Eq, Show)
 
-- ----------------------------------------
-- semantic domains
 
data Result a = Err { err :: String }
              | Val { val ::    [a] }
                deriving (Eq, Show)

-- ----------------------------------------
-- class instances

instance Monad Result where
  return x      = Val [x]
  Err msg >>= _ = Err msg
  Val []  >>= _ = Val []
  Val xs  >>= g = Val . concat . map (val . g) $ xs


instance MonadError String Result where
  throwError msg         = Err msg
  catchError (Val   a) _ = Val a
  catchError (Err msg) f = f msg
 
-- ----------------------------------------
-- the meaning of an expression
 
eval :: Expr -> Result Int
eval (Const i) = return i 
eval (Binary op l r) = lookupMft op >>= (\f -> (eval l) `f` (eval r))
 
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
    , (Div, divM)
    , (PlusMinus, plusMinusM)
    ]

plusMinusM ma mb = do
  x <- ma
  y <- mb
  Val [x+y,x-y]

divM ma mb = do
  x <- ma
  y <- mb
  if y == 0 
  then throwError "division by 0"
  else return (x `div` y)


-- ----------------------------------------
-- sample expressions
 
e1 = eval $ Binary Mul (Binary Add (Const 4)
                                   (Const 2))
                       (Const 7)
e2 = eval $ Binary PlusMinus (Const 4) (Const 2)
e3 = eval $ Binary Mod (Const 6) (Const 2)
e4 = eval $ Binary Div (Const 10) (Const 0)
e5 = eval $ Binary Div (Const 10) (Const 2)
