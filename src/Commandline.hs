{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Commandline where

import Language.UHIM.Dictionary.SKK.SKKExtended (SKKDict)
import qualified Language.UHIM.Dictionary.SKK.SKKExtended as SKK
import Language.UHIM.Dictionary.Transform.TUTYomi as TTY
import Language.UHIM.Dictionary.Transform.Variants as Var
import Language.UHIM.Dictionary.Yaml

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.List as L
import Data.Monoid
import Text.Parsec
import GHC.Generics
-- import Control.Lens

import Control.Monad.Except

type Variable = String

data Expr = EVar Variable
          | ELiteral Value
          | ETuple [Expr]
          | EExtractSKKYomi Expr
          | EExtractSKKVariant Expr
          | EUnion Expr Expr
  deriving (Show, Read, Ord, Eq)

data Command = SetVar [Variable] Expr
             | UnsetVar [Variable]
             | ReadYaml Variable Expr
             | ReadSKK Variable Expr
             | WriteYaml Expr Expr
             | WriteSKK Expr Expr
  deriving (Show, Read, Ord, Eq)

data Value = Tuple [Value]
           | Path FilePath
           | String String
           | ByteString BS.ByteString
           | LazyByteString BL.ByteString
           | Dictionary Dictionary
           | SKKDictionary SKK.SKKDict
  deriving (Show, Read, Ord, Eq)

type Environment = Map String Value

getVar :: Environment -> Variable -> Maybe Value
getVar = flip M.lookup 

setVar :: Variable -> Value -> Environment -> Environment
setVar = M.insert

unsetVar :: Variable -> Environment -> Environment
unsetVar = M.delete

evalExpr :: Monad m => Environment -> Expr -> m Value
-- evalExpr :: Environment -> Expr -> Value
evalExpr env (EVar v) = maybe (fail $ "Variable " ++ v ++ " not found.") return $ getVar env v
evalExpr _   (ELiteral v) = return v
evalExpr env (ETuple [e]) = evalExpr env e
evalExpr env (ETuple es) = Tuple <$> mapM (evalExpr env) es
evalExpr env (EExtractSKKYomi e) = do
  (Dictionary d) <- evalExpr env e
  return . SKKDictionary $ TTY.extractSKK TTY.defaultConfig d
evalExpr env (EExtractSKKVariant e) = do
  (Dictionary d) <- evalExpr env e
  return . SKKDictionary $ Var.extractSKK Var.defaultConfig d
evalExpr env (EUnion e1 e2) = do
  d1 <- evalExpr env e1
  d2 <- evalExpr env e2
  return $ unionDictionary d1 d2

unionDictionary :: Value -> Value -> Value
unionDictionary (SKKDictionary d1) (SKKDictionary d2) = SKKDictionary $ SKK.union d1 d2

evalCommand :: Environment -> Command -> IO Environment
evalCommand env (SetVar vs e) = f vs <$> evalExpr env e
  where
    f [] (Tuple []) = env
    f [v] value = setVar v value env
    f vs (Tuple values) | length vs == length values = foldr (uncurry setVar) env $ zip vs values
evalCommand env (UnsetVar vs) = return $ foldr unsetVar env vs
evalCommand env (ReadYaml v fp) = do
  (Path fp') <- evalExpr env fp
  value <- either (error . show) id <$> readFromFile fp'
  return $ setVar v (Dictionary value) env
evalCommand env (WriteYaml d fp) = do
  (Dictionary d') <- evalExpr env d
  (Path fp') <- evalExpr env fp
  writeToFile d' fp'
  return env
evalCommand env (WriteSKK d fp) = do
  (SKKDictionary d') <- evalExpr env d
  (Path fp') <- evalExpr env fp
  writeFile fp' $ SKK.emitSKKDictionary SKK.defaultConfig d'
  return env

parseExpr [] = []

parseCommand [] = []
parseCommand ("input":"yaml":v:f:xs) = ReadYaml v (ELiteral $ Path f) : parseCommand xs
parseCommand ("output":"yaml":v:f:xs) = ReadYaml v (ELiteral $ Path f) : parseCommand xs
