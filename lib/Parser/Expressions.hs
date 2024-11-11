module Parser.Expressions where

import           Parser.Utils
import           Types.AST
import           Types.Lexer
import           Types.Parser (Precedence)
import           Types.Repl

parseExpr :: Precedence -> Repl Expr
parseExpr pr = do
  cur <- peek
  left <- getNud (tokenKind cur)
  go left
  where
    go :: Expr -> Repl Expr
    go left' = do
      op <- peek
      let opPr = getPrecedencs (tokenKind op)
      case opPr <= pr of
        True -> return left'
        False -> do
          case getLed (tokenKind op) of
            Nothing    -> return left'
            (Just led) -> led left' >>= go

parseLiteral :: Repl Expr
parseLiteral = do
  cur <- peek
  case tokenKind cur of
    IntToken -> advance >> (return $ IntExpr ((read . tokenValue $ cur) :: Int))
    _        -> error $ "Can not parse " <> show cur

getNud :: TokenKind -> Repl Expr
getNud IntToken = parseLiteral
getNud token    = error $ "can not parse " <> show token

getLed :: TokenKind -> Maybe (Expr -> Repl Expr)
getLed PlusToken  = Just parseBinaryExpr
getLed MinusToken = Just parseBinaryExpr
getLed StarToken  = Just parseBinaryExpr
getLed SlashToken = Just parseBinaryExpr
getLed token      = Nothing

parseBinaryExpr :: Expr -> Repl Expr
parseBinaryExpr left = do
  cur <- peek
  advance
  right <- parseExpr (getPrecedencs $ tokenKind cur)
  return $ BinaryExpr (tokenKind cur) left right
