module Parser.Parser where

import           Parser.Expressions
import           Parser.Statements
import           Types.AST
import           Types.Lexer
import           Types.Parser
import           Types.Repl

parse :: Repl Stmt
parse = do
  stmts <- parseBlockStmt EOFToken
  return $ BlockStmt "Program" stmts
