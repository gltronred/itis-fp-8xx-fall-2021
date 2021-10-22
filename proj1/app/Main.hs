{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Except
import Control.Monad.State
import Data.Maybe

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Number Int
  | Var String
  | Assign String Expr
  deriving (Eq,Show,Read)

type Env = [(String, Int)]

myConst :: Monad m => Int -> m Int
myConst x = pure x

onError :: MonadError String m
        => Bool -> String -> m ()
onError b msg = if b
                then throwError msg
                else pure ()

eval :: ( MonadError String m
        , MonadState Env m )
     => Expr -> m Int
eval expr = case expr of
  Add e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    pure $ v1 + v2
  Sub e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    pure $ v1 - v2
  Mul e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    pure $ v1 * v2
  Div e1 e2 -> do
    v1 <- eval e1
    v2 <- eval e2
    onError (v2 == 0) "Division by 0"
    pure $ v1 `div` v2
  Number n -> myConst n
  Var name -> do
    env <- get
    onError (isNothing $ lookup name env) $
      "No variable " ++ name
    let Just v = lookup name env
    pure v
  Assign name e1 -> do
    v1 <- eval e1
    modify $ \st -> (name,v1) : filter (\v -> fst v /= name) st
    pure v1

run :: Expr -> Env -> Either String (Int, Env)
run expr env = runExcept $ runStateT ev env
  where ev :: StateT Env (Except String) Int
        ev = eval expr

main :: IO ()
main = do
  let expr = Add (Var "x") (Number 2)
  print $ run expr []
  print $ run expr [("x", 2)]
  print $ run (Div (Var "x") (Number 0)) [("x", 2)]
  let assign = Assign "x" (Number 5)
  print $ run (Add assign (Var "x")) [("x", 2)]
  print $ run (Add (Var "x") assign) [("x", 2)]
