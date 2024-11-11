module Parser.Statements where

import           Control.Monad
import           Parser.Expressions
import           Parser.Utils
import           Types.AST
import           Types.Lexer
import           Types.Parser
import           Types.Repl
import           Utils.Utils

parseBlockStmt :: TokenKind -> Repl [Stmt]
parseBlockStmt endToken = do
  cur <- peek
  let action
        | tokenKind cur == endToken = return []
        | tokenKind cur == SemiColonToken = advance >> (parseBlockStmt endToken)
        | otherwise = do
            expr <- parseExprStmt
            consume SemiColonToken "SemiColon expected after statement"
            rest <- parseBlockStmt endToken
            return (expr : rest)
  action

parseExprStmt :: Repl Stmt
parseExprStmt = parseExpr Default >>= return . ExprStmt
