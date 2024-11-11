module Lexer.Error where

import           Control.Monad.Trans.RWS
import           Debug.Trace
import           Types.Error
import           Types.Repl
import           Utils.PrettyPrint
import           Utils.Utils

lexerErrorHandler :: Error -> Repl ()
lexerErrorHandler (LexerError (l, c, len) msg) = do
  src <- traceShow (l, c, len) $ getErrorSource l c len
  tell [concat [color Red $ "[Lexer] Error at line " <> show l <> ", col " <> show c, "\n", msg, "\n", color Yellow "[Source code] ", src]]
