module Lexer.Lexer where

import           Control.Monad.Catch
import           Control.Monad.Trans.RWS
import           Data.Char               (isAlpha, isAlphaNum, isDigit)
import qualified Data.Text               as T (drop, index, length, take,
                                               unpack)
import qualified Data.Vector             as V (snoc)
import           Lexer.Error
import           Types.Error
import           Types.Lexer
import           Types.Repl
import           Utils.Utils             (todo)

tokenize :: Repl ()
tokenize = do
  cur <- peek
  case cur of
    '\0' -> sync >> advance >> pushToken EOFToken
    '\n' -> advanceNL >> tokenize
    ' ' -> advance >> tokenize
    _ -> sync >> advance >> catch (consumeToken cur) lexerErrorHandler >> tokenize

peek :: Repl Char
peek = do
  (_, cur, _, _) <- gets lexerPos
  src <- ask
  if cur >= T.length src then return '\0' else return $ T.index src cur

consumeToken :: Char -> Repl ()
consumeToken cur
  | isDigit cur = numberToken >> pushToken IntToken
  | cur == '+' = pushToken PlusToken
  | cur == '*' = pushToken StarToken
  | cur == '-' = pushToken MinusToken
  | cur == '/' = pushToken SlashToken
  | cur == ';' = pushToken SemiColonToken
  | cur == '(' = pushToken OpenParen
  | cur == ')' = pushToken CloseParen
  | otherwise = getTokenPos >>= \tp -> (throwM $ LexerError tp ("Unknown character '" <> [cur] <> "'"))

pushToken :: TokenKind -> Repl ()
pushToken tk = do
  tp <- getTokenPos
  v <- getTokenValue
  modify (\m -> m {tokens = V.snoc (tokens m) (Token tk tp v)})

getTokenValue :: Repl String
getTokenValue = do
  (s, cur, _, _) <- gets lexerPos
  ask >>= return . T.unpack . T.take (cur - s) . T.drop s

getTokenPos :: Repl TokenPos
getTokenPos = do
  (s, cur, l, c) <- gets lexerPos
  return $ (l, c - (cur - s), (cur - s))

numberToken :: Repl ()
numberToken = do
  cur <- peek
  case isDigit cur of
    True  -> advance >> numberToken
    False -> return ()

advance, advanceNL :: Repl ()
advance = gets lexerPos >>= \(s, cur, l, c) -> modify (\m -> m {lexerPos = (s, cur + 1, l, c + 1)})
advanceNL = gets lexerPos >>= \(s, cur, l, _) -> modify (\m -> m {lexerPos = (s, cur + 1, l + 1, 1)})

-- sync: fucnction to synchronice start position of previous token to current position of lexer
sync :: Repl ()
sync = gets lexerPos >>= \(_, cur, l, c) -> modify (\m -> m {lexerPos = (cur, cur, l, c)})

-- list of keywords
