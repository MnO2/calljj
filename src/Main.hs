{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.IntMap.Strict                     as Map
import qualified Data.Text                              as T
import           System.Console.Haskeline
import qualified Text.Parsec                            as P
import           Text.Parsec ((<|>), ParsecT, Stream)
import qualified Text.Parsec.Char                       as P


data Inst = Abs Int [Inst]
          | App Int Int
          deriving (Eq, Show)

data Prog = Prog {
                body :: [Inst]
            }
            deriving (Eq, Show)

data Value = Fn
           | Succ
           | In
           | Out
           deriving (Eq, Show)

type Code = [Inst]
type Environment = Map.IntMap Value
type Dump = [(Code, Environment)]

data Machine = Machine Code Environment Dump




initialEnv :: Environment
initialEnv = Map.fromList [(1, In), (2, Out), (3, Succ)]


initialDump :: Dump
initialDump = [([], Map.empty), ([App 1 1], Map.empty)]


start :: Prog -> IO ()
start prog = do
  let machine = Machine (body prog) initialEnv initialDump
  eval machine


eval :: Machine -> IO ()
eval machine = undefined


absParser :: Stream s m Char => ParsecT s u m Inst
absParser = do { arity_seq <- aritySeqParser
               ; let arity = length arity_seq
               ; insts <- P.count arity appParser
               ; return $ Abs arity insts
               }
  where
    aritySeqParser :: Stream s m Char => ParsecT s u m [Char]
    aritySeqParser = P.many1 (P.char '解')


appParser :: Stream s m Char => ParsecT s u m Inst
appParser = do { idx_of_function_seq <- idxOfFunctionParser
               ; idx_of_argument_seq <- idxOfArgumentParser
               ; let idx_of_function = length idx_of_function_seq
               ; let idx_of_argument = length idx_of_argument_seq
               ; return $ App idx_of_function idx_of_argument
               }
  where 
    idxOfFunctionParser :: Stream s m Char => ParsecT s u m [Char]
    idxOfFunctionParser = P.many1 (P.char '接')

    idxOfArgumentParser :: Stream s m Char => ParsecT s u m [Char]
    idxOfArgumentParser = P.many1 (P.char '解')


instParser :: Stream s m Char => ParsecT s u m Inst
instParser = appParser <|> absParser


programParser :: Stream s m Char => ParsecT s u m Prog
programParser = do { insts <- instParser `P.sepBy1` (P.string "叫我")
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
          case (P.parse programParser "" input) of
            Left err -> liftIO $ print err
            Right prog -> liftIO $ start prog
          loop
