module Types.AST where

import           Types.Lexer

type BlockName = String

data Stmt = ExprStmt Expr
          -- BlockName - for pretty print, for example main block will be "Main Program"
          | BlockStmt BlockName [Stmt]
  deriving (Show)

data Expr = IntExpr Int
          | BinaryExpr TokenKind Expr Expr
  deriving (Show)
