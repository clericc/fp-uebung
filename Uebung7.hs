{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- Uebung 6 Funktionale Programmierung
-- Von Julian Wefers und Alexander Mills
 
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
 
import Control.Monad ()         -- ( liftM2 )
import Control.Monad.Error
 
-- ----------------------------------------
-- syntactic domains
 
data Expr  = Const  Int
           | Var    Id                -- NEU
           | Let    Id    Expr Expr   -- NEU
           | Binary BinOp Expr Expr
             deriving (Show)
 
data BinOp = Add | Sub | Mul | Div | Mod
             deriving (Eq, Show)
 
type Id    = String
 
-- ----------------------------------------
-- semantic domains

data Result a
           = Res { unRes :: Env -> ResVal a }
 
data ResVal a
           = Val { val :: a }
           | Exc { exc :: String }
             deriving (Show)
 
type Env   = [(Id, Int)]



instance Monad Result where
  return        = Res . const . Val
  (Res f) >>= g = Res $ \ env -> case f env of
                                   (Val v) -> unRes (g v) env
                                   err     -> err

{-

instance MonadError String Result where
  throwError s  = ...
  catchError (Res f) handler
                = ...
 
instance MonadReader Env Result where
  ask       = ...
  local f c = ...
  

instance MonadError String Result where
  throwError     = Val . return . Left
  catchError     = undefined
  
-- ----------------------------------------
-- the meaning of an expression
 
eval :: Expr -> Result Int
eval (Const i)
  = return i
 
eval (Binary op l r)
  = do
    mf <- lookupMft op
    mf (eval l) (eval r)
 
-- ----------------------------------------
-- the meaning of binary operators
 
type MF = Result Int -> Result Int ->
          Result Int
 
lookupMft :: BinOp -> Result MF
lookupMft op
  = case lookup op mft of
    Nothing -> throwError
               "operation not implemented"
    Just mf -> return mf
    
mft :: [(BinOp, MF)]
mft
  = [ (Add, liftM2 (+))
    , (Sub, liftM2 (-))
    , (Mul, liftM2 (*))
    , (Div, divM)
    , (PlusMinus, plusMinusM)
    ]

plusMinusM :: MF
plusMinusM ma mb = do
  x <- ma
  y <- mb
  Val [x+y,x-y]

divM :: MF
divM ma mb = do
  x <- ma
  y <- mb
  if y == 0 
  then Val []
  else return (x `div` y)
 

 
 
-- ----------------------------------------
-- sample expressions
 
e1 = Binary Mul (Binary Add (Const 4)
                       (Const 2))
                       (Const 7)
e2 = Binary PlusMinus (Const 4) (Const 2)
e3 = Binary Mod (Const 6) (Const 2)
e4 = Binary Div (Const 10) (Const 0)
e5 = Binary Div (Const 10) (Const 2)
e6 = Binary PlusMinus (e2) (Const 2)
e7 = Binary Div (Const 100) (e6)
 
-- ----------------------------------------

-}