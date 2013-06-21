{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- Uebung 7 Funktionale Programmierung
-- Von Julian Wefers und Alexander Mills
-- ----------------------------------------

module Expr7 where

import Data.Maybe ( fromMaybe )

import Control.Monad ( liftM2 )
import Control.Monad.Error ( MonadError ( .. ) )
import Control.Monad.State ( MonadState ( .. ) )

-- ----------------------------------------
-- syntactic domains

data Expr  = Const  Int
           | Var    Id
           | Assign Id    Expr
           | If     Expr  Expr Expr
           | While  Expr  Expr
           | Binary BinOp Expr Expr
             deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod | Seq
             deriving (Eq, Show)

type Id    = String

-- ----------------------------------------
-- semantic domains

newtype Result a
           = Res { unRes :: VState -> (ResVal a, VState) }

data ResVal a
           = Val { val :: a }
           | Exc { exc :: String }
             deriving (Show)

type VState   = [(Id, Int)]

instance Monad ResVal where
  return        = Val
  (Exc e) >>= _ = Exc e
  (Val v) >>= g = g v


instance Monad Result where
  -- Wrap a value in ResVal ignore any incoming environment since the value is constant
  return x      = Res $ \ state -> (return x, state)

  (Res f) >>= g = Res $ \ state -> let (val,state') = f state 
                                   in  case val of
                                         (Val   v) -> unRes (g v) state'
                                         (Exc msg) -> (Exc msg, state')


instance MonadError String Result where
  -- wrap an exception string and ignore any environment
  throwError  msg  = Res $ \ state -> (Exc msg, state)

  -- If f throws an Error, the handler is applied, otherwise ignored
  catchError (Res f) handler
                = Res $ \ state -> case f state of
                                     (Exc e, state') -> unRes (handler e) state'
                                     (Val v, state') -> (Val v, state')


instance MonadState VState Result where
  -- wrap any environment into the monad
  get       = Res $ \ state -> (return state, state)

  -- ignore the existing state, replacing it with the new one
  put state = Res $ \ _ -> (return (), state)

  -- wrap a non-monad state action into the Monad
  state f   = Res $ \ state -> let (val, state') = f state
                               in  (return val, state')

-- ----------------------------------------
-- the meaning of an expression

eval :: Expr -> Result Int

eval (Const i)
  = return i

-- try to retrieve a value for id out of the current env
eval (Var  v)
  = get >>= \ state ->
      case lookup v state of
        Nothing  -> throwError "unbound variable"
        Just val -> return val

eval (Assign v e) = do
  val   <- eval e
  state <- get
  put ((v,val):state)
  return val

eval (If cond e1 e2) = do
  cond' <- eval cond
  if   cond' /= 0
  then eval e1
  else eval e2

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
    , (Div,       divM)
    , (Seq,       (>>))
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
  if   y == 0
  then throwError "division by zero"
  else return (x `div` y)

-- ----------------------------------------
-- expression evaluator with outer environment

evalEnv     :: Expr -> VState -> ResVal Int
evalEnv e s  = fst $ unRes (eval e) s

-- ----------------------------------------
-- sample expressions

e1 = Binary Mul (Binary Add (Const 4)
                       (Const 2))
                       (Const 7)
e3 = Binary Mod (Const 6) (Const 2)
e4 = Binary Div (Const 10) (Const 0)
e5 = Binary Div (Const 10) (Const 2)
-- ----------------------------------------
