module Repl.Repl where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.RWS
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T (readFile)
import qualified Data.Vector             as V (empty, toList)
import           Lexer.Lexer
import           Parser.Parser
import           System.Directory
import           Types.Repl
import           Utils.PrettyPrint

memory :: Memory
memory = Memory {lexerPos = (0, 0, 1, 1), parserPos = 0, source = T.pack "", tokens = V.empty}

runRepl :: IO ()
runRepl = do
  _ <- runRWST repl (T.pack "") memory
  putStrLn "Exit CUTE. Bye! See you again!"

repl :: Repl ()
repl = do
  put memory
  i <- getInput
  case T.unpack i of
    ":exit" -> return ()
    _ -> local (const i) $ do
      (_, w) <- listen tokenize
      ts <- gets tokens
      liftIO $ mapM_ (putStrLn . show) (V.toList ts)
      liftIO $ mapM_ putStrLn w
      case length w == 0 of
        False -> (liftIO $ putStrLn $ color Cyan "\nPlease fix errors to procced to parsing\n") >> repl
        True -> do
          program <- parse
          liftIO $ putStrLn . show $ program
          repl

getInput :: Repl Text
getInput = do
  liftIO $ putStr ">:> "
  i <- liftIO getLine
  let action
        | take 3 i == ":l " =
            getFileContent (drop 3 i) >>= \case
              (Just t) -> return t
              Nothing  -> getInput
        | take 6 i == ":load " =
            getFileContent (drop 6 i) >>= \case
              (Just t) -> return t
              Nothing  -> getInput
        | otherwise = return $ T.pack i
  action

getFileContent :: FilePath -> Repl (Maybe Text)
getFileContent fp = do
  liftIO $
    doesFileExist fp >>= \case
      True -> (liftIO $ T.readFile fp) >>= (return . Just)
      False -> (liftIO $ putStrLn $ color Red "Error: " <> "File '" <> fp <> "' doesn't exist") >> return Nothing
