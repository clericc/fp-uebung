{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- Uebung 8 Funktionale Programmierung
-- Von Julian Wefers und Alexander Mills
-- ----------------------------------------

module Expr8 where

-- import Data.Maybe ( fromMaybe )

import Control.Monad       ( liftM2 )
import Control.Monad.Error ( MonadError ( .. ) )
import Control.Monad.State ( MonadState ( .. ) )
import Data.AssocList      ( addEntry )
import Control.Monad.Loops ( whileM )
import Control.Monad.IO.Class    ( MonadIO ( .. ) )

-- ----------------------------------------
-- syntactic domains

data Expr  = Const  Int
           | Var    Id
           | Assign Id    Expr
           | If     Expr  Expr Expr
           | While  Expr  Expr
           | Binary BinOp Expr Expr
           | Read
           | Write  String Expr
             deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod | Seq
             deriving (Eq, Show)

type Id    = String

-- ----------------------------------------
-- semantic domains

newtype Result a
           = Res { unRes :: VState -> IO (ResVal a, VState) }

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
  return x      = Res $ \ state -> return (return x, state)

  (Res f) >>= g = Res $ \ state -> f state >>= \ (val,state') ->
                                       case val of
                                         (Val   v) -> unRes (g v) state'
                                         (Exc msg) -> return (Exc msg, state')

  (Res f) >>= g = Res $ \ state -> f state >>= \ (val, state') -> 
                                       case val of
                                         (Val   v) -> unRes (g v) state'
                                         (Exc msg) -> return (Exc msg, state')

instance MonadError String Result where
  -- wrap an exception string and ignore any environment
  throwError  msg  = Res $ \ state -> return (Exc msg, state)

  -- If f throws an Error, the handler is applied, otherwise ignored
  catchError (Res f) handler
                   = Res $ \ state -> f state >>= \ (val, state') -> case val of
                                        (Exc e) -> unRes (handler e) state'
                                        (Val v) -> return (Val v, state')


instance MonadState VState Result where
  -- wrap any environment into the monad
  get       = Res $ \ state -> return (return state, state)

  -- ignore the existing state, replacing it with the new one
  put state = Res $ \ _ -> return (return (), state)

  -- wrap a non-monad state action into the Monad
  state f   = Res $ \ state -> let (val, state') = f state in return (return val, state')


instance MonadIO Result where
  -- extract the value from the IO action and rewrap it into Result, keeping the IO state
  liftIO a  = Res $ \ state -> a >>= \ a' -> return (return a', state)

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
  put $ addEntry v val state
  return val

eval (If cond e1 e2) = do
  cond' <- eval cond
  if   cond' /= 0
  then eval e1
  else eval e2

-- Repeat an action until cond is zero.
-- Returns the value of the last execution of the expr block.
eval (While cond expr) 
  = whileM (eval cond >>= return . (/= 0)) (eval expr) >>= return . last

{-
  = evalWhile cond expr 0
    where
    evalWhile cond expr val = do
      cond' <- eval cond
      if cond' /= 0
      then eval expr >>= evalWhile cond expr 
      else return val
-}

eval (Binary op l r)
  = lookupMft op >>= \ mf -> mf (eval l) (eval r)

eval (Read)
  = liftIO readLn

eval (Write msg expr) = do
  val <- eval expr
  liftIO . print $ msg ++ (show val)
  return val


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

evalEnv     :: Expr -> VState -> IO (ResVal Int)
evalEnv e s  = unRes (eval e) s >>= return . fst

-- ----------------------------------------
-- sample expressions

e1 = Binary Mul (Binary Add (Const 2)
                            (Const 4)
                )
                (Const 7)
e2 = Binary Div (Const 1) (Const 0)
e3 = Binary Mod (Const 1) (Const 0)
e4 = Var "x"
e5 = Binary Mul (Binary Add e4
                            (Const 1)
                ) e4
 
e4' = Binary Seq (Assign "x" (Const 42)) e4
e5' = Binary Seq (Assign "x" (Const 6))  e5
 
e6' = foldr1 (Binary Seq) $
      [ Assign "x" (Const 42)
      , Assign "y" (Const 13)
      , Assign "t" (Var "x")
      , Assign "x" (Var "y")
      , Assign "y" (Var "t")
      ]
 
e7' = foldr1 (Binary Seq) $
      [ e6'
      , Assign "r" (Binary Sub (Var "x") (Var "y"))
      , If (Var "r") (Var "r") (Var "x")
      ]
 
e8  = Binary Seq e4' $
      While (Var "x")
            (Assign "x" (Binary Sub (Var "x")
                                    (Const 1)))

e9 = foldr1 (Binary Seq) $
      [ e6'
      , Assign "r" (Binary Sub (Var "x") (Var "y"))
      , If (Const 0) (Var "r") (Var "x")
      ]

e10 = foldr1 (Binary Seq) $
        [ Assign "x" (Const 10)
        , Assign "y" (Const  5)
        , While (Var "y")
            (Binary Seq
              (Assign "y" (Binary Sub (Var "y")
                                    (Const 1)))
              (Assign "x" (Binary Sub (Var "x")
                                    (Const 1))))
        ]

-- ----------------------------------------
