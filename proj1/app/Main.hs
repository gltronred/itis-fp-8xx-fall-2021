module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Maybe

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Number Int
  | Var String
  deriving (Eq,Show,Read)

type Env = [(String, Int)]

eval :: Expr -> ReaderT Env Maybe Int
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
    pure $ v1 `div` v2
  Number n -> pure n
  Var name -> do
    env <- ask
    lift $ lookup name env

main :: IO ()
main = do
  let expr = Add (Var "x") (Number 2)
  print $ runReaderT (eval expr) []
  print $ runReaderT (eval expr) [("x", 2)]
  print $ runReader (eval $ Div (Var "x") (Number 0)) [("x", 2)]
