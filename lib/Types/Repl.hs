module Types.Repl where

import           Control.Monad.Catch
import           Control.Monad.Trans.RWS
import           Data.Text               (Text)
import           Data.Vector             (Vector)
import           Types.Common
import           Types.Lexer
import           Types.Parser

data Memory = Memory { source    :: Text
                     , lexerPos  :: LexerPos
                     , parserPos :: ParserPos
                     , tokens    :: Vector Token
                     }

type Repl a = RWST Text [Message] Memory IO a
