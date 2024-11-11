module Types.Lexer where

import           Types.Common

-- LexerPos (start position of token, current position in source, current line, start position in the line)
type LexerPos = (Col, Col, Line, Col)

-- TokenPos (line, column, length)
type TokenPos = (Line, Col, Length)

data TokenKind = IntToken | PlusToken | MinusToken | StarToken | SlashToken | SemiColonToken | OpenParen | CloseParen | EOFToken deriving
  ( Eq
  , Show
  )

data Token = Token { tokenKind  :: TokenKind
                   , tokenPos   :: TokenPos
                   , tokenValue :: String
                   }

instance Show Token where
  show (Token tk (l, c, len) tv) = concat ["TOKEN <", show tk, ">, line ", show l, ", col ", show c, "-", show (c + len - 1), ", value '", tv, "'"]
