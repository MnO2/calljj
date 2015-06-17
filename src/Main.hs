{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text                              as T
import           System.Console.Haskeline
import           Text.Parsec (sepBy1, many1, (<|>), parse, ParsecT, Stream)
import           Text.Parsec.Char (string, char)


data Inst = Abs Int [Inst]
          | App Int Int
          deriving (Eq, Show)

data Prog = Prog [Inst]


eval :: Prog -> IO ()
eval = undefined


absParser :: Stream s m Char => ParsecT s u m Inst
absParser = do { arity_seq <- aritySeqParser
               ; let arity = length arity_seq
               ; return $ Abs arity []
               }
  where
    aritySeqParser :: Stream s m Char => ParsecT s u m [Char]
    aritySeqParser = many1 (char '解')


appParser :: Stream s m Char => ParsecT s u m Inst
appParser = do { idx_of_function_seq <- idxOfFunctionParser
               ; return $ App 3 3
               }
  where 
    idxOfFunctionParser :: Stream s m Char => ParsecT s u m [Char]
    idxOfFunctionParser = many1 (char '接')

    idxOfArgumentParser :: Stream s m Char => ParsecT s u m [Char]
    idxOfArgumentParser = many1 (char '解')


instParser :: Stream s m Char => ParsecT s u m Inst
instParser = appParser <|> absParser


programParser :: Stream s m Char => ParsecT s u m Prog
programParser = do { insts <- instParser `sepBy1` (string "叫我")
                   ; return $ Prog insts
                   }


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
