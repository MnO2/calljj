module Main where

import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
import Text.Parsec (sepBy1, many1, (<|>), parse)
import Text.Parsec.Char (string, char)

data Inst = Abs Int [Inst]
          | App Int Int
          deriving (Eq, Show)

data Prog = Prog [Inst]


eval :: Prog -> IO ()
eval = undefined


absParser = many1 (char '解')

appParser = many1 (char '接')

instParser = appParser <|> absParser

programParser = instParser `sepBy1` (string "叫我")


main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "% "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do 
          outputStrLn $ "Input was: " ++ input
          case (parse programParser "" input) of
            Left err -> liftIO $ print err
            Right xs -> liftIO $ putStrLn "Hello Word"
          loop
