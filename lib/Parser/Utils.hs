module Parser.Utils where

import           Control.Monad.Trans.RWS
import qualified Data.Vector             as V (length, (!))
import           Types.AST
import           Types.Lexer
import           Types.Parser            (Precedence (Additive, Default, Multiplicative))
import           Types.Repl

consume :: TokenKind -> String -> Repl ()
consume tk msg = do
  cur <- peek
  if tokenKind cur == tk then return () else error msg

peek :: Repl Token
peek = do
  ts <- gets tokens
  p <- gets parserPos
  let l = V.length ts
  if p < l then return $ (V.!) ts p else return $ (V.!) ts (p - 1)

advance :: Repl ()
advance = modify (\m -> m {parserPos = 1 + parserPos m})

getPrecedencs :: TokenKind -> Precedence
getPrecedencs PlusToken  = Additive
getPrecedencs MinusToken = Additive
getPrecedencs StarToken  = Multiplicative
getPrecedencs SlashToken = Multiplicative
getPrecedencs _          = Default
