module Types.Error where

import           Control.Exception
import           Types.Common
import           Types.Lexer

data Error = LexerError TokenPos Message
  deriving (Show)

instance Exception Error
