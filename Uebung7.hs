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
 
import Data.Maybe ( fromMaybe )
 
import Control.Monad ( liftM2 )
import Control.Monad.Error ( MonadError ( .. ) )
import Control.Monad.Reader ( MonadReader ( .. ) )

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

newtype Result a
           = Res { unRes :: Env -> ResVal a }
 
data ResVal a
           = Val { val :: a }
           | Exc { exc :: String }
             deriving (Show)
 
type Env   = [(Id, Int)]

instance Monad ResVal where
  return        = Val
  (Exc e) >>= _ = Exc e
  (Val v) >>= g = g v

instance Monad Result where
  return        = Res . const . return
  (Res f) >>= g = Res $ \ env -> f env >>= \ v -> unRes (g v) env

instance MonadError String Result where
  throwError  = Res . const . Exc
  catchError (Res f) handler
                = Res $ \ env -> case f env of
                                   (Exc e) -> unRes (handler e) env
                                   (Val v) -> Val v
 
instance MonadReader Env Result where
  ask       = Res Val
  local f c = Res (unRes c . f)
  reader f  = Res (return . f)

instance Show a => Show (Result a) where
   show (Res f) = show $ f []

-- ----------------------------------------
-- the meaning of an expression

eval :: Expr -> Result Int
eval (Const i) 
  = return i

eval (Var  id)
  = ask >>= \ env ->
      case lookup id env of
        Nothing  -> throwError "unbound variable"
        Just val -> return val

eval (Let id e1 e2)
  = eval e1 >>= \ val -> local ((id,val):) (eval e2) 

eval (Binary op l r)
  = lookupMft op >>= \ mf -> mf (eval l) (eval r)

-- ----------------------------------------
-- the meaning of binary operators
 
type MF = Result Int -> Result Int -> Result Int
 
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
    ]

--plusMinusM :: MF
--plusMinusM ma mb = do
--  x <- ma
--  y <- mb
--  Val [x+y,x-y]

divM :: MF
divM ma mb = do
  x <- ma
  y <- mb
  if y == 0 
  then throwError "division by zero"
  else return (x `div` y)
 

 
 
-- ----------------------------------------
-- sample expressions
 
e1 = Binary Mul (Binary Add (Const 4)
                       (Const 2))
                       (Const 7)
e3 = Binary Mod (Const 6) (Const 2)
e4 = Binary Div (Const 10) (Const 0)
e5 = Binary Div (Const 10) (Const 2)
e6 = Let "a" (Const 5) (Var "a")
e7 = Let "a" (Let "b" (Const 1) (Var "b")) (Let "b" (Const 2) (Binary Add (Var "a") (Var "b")))
 
-- ----------------------------------------
