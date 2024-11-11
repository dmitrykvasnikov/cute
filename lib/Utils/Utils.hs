module Utils.Utils where

import           Control.Monad.Trans.RWS
import qualified Data.Text               as T (lines, unpack)
import           Types.Common
import           Types.Repl
import           Utils.PrettyPrint

getErrorSource :: Line -> Col -> Length -> Repl String
getErrorSource l c len = do
  str <- (T.unpack . (flip (!!) (l - 1)) . T.lines) <$> ask
  return $ concat [take (c - 1) str, (color Red . style Bold) (take len $ drop (c - 1) str), drop (c + len - 1) str]

todo = undefined
