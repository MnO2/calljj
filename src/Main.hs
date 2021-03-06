{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict       as S
import           Data.Char (ord, chr)
import qualified Data.IntMap.Strict                     as Map
import           Data.Maybe (fromJust)
import qualified Data.Text                              as T
import           System.Console.Haskeline
import qualified Text.Parsec                            as P
import           Text.Parsec ((<|>), ParsecT, Stream)
import qualified Text.Parsec.Char                       as P


type Arity = Int

data Inst = Abs Arity [Inst]
          | App Int Int
          deriving (Eq, Show)

data Prog = Prog 
            {
                body :: [Inst]
            }
            deriving (Eq, Show)

data Value = Fn Arity Code Environment
           | CharFn Char
           | Succ
           | In
           | Out
           deriving (Eq, Show)

type Code = [Inst]
type Environment = Map.IntMap Value
type Dump = [(Code, Environment)]

data Machine = Machine 
              {
                code :: Code
              , environment :: Environment
              , dump :: Dump
              }


type SECD a = StateT Machine IO a


printCode :: Code -> IO ()
printCode c = do
  putStr "curr code: "
  print c


printEnv :: Environment -> IO ()
printEnv env = do
  putStr "curr environment: "
  print $ Map.toList env


printInst :: Inst -> IO ()
printInst inst = do
  putStr "inst: "
  print $ inst


initialEnv :: Environment
initialEnv = Map.fromList [(1, In), (2, CharFn '姐'), (3, Succ), (4, Out)]


initialDump :: Dump
initialDump = [([], Map.empty), ([App 1 1], Map.empty)]


start :: Prog -> IO ()
start prog = do
  let machine = Machine (body prog) initialEnv initialDump
  ret <- S.evalStateT eval machine
  return ret


evalInst :: Inst -> SECD () 
evalInst (App m n) = do { machine <- S.get
                        ; let env = environment machine
                        ; let top = Map.size env
                        ; let f = fromJust $ Map.lookup (top-m+1) env
                        ; let v = fromJust $ Map.lookup (top-n+1) env
                        ; case f of
                              Fn fn_arity fn_code fn_env ->  do
                                                                if fn_arity >= 1
                                                                then do
                                                                        let dump' = ((code machine, environment machine):(dump machine))
                                                                        let fn_env' = Map.insert ((Map.size fn_env)+1) v fn_env
                                                                        let machine' = Machine [Abs fn_arity fn_code] fn_env' dump'
                                                                        S.put machine'
                                                                else do
                                                                        let dump' = ((code machine, environment machine):(dump machine))
                                                                        let fn_env' = Map.insert ((Map.size fn_env)+1) v fn_env
                                                                        let machine' = Machine fn_code fn_env' dump'
                                                                        S.put machine'
                              In -> do
                                     c <- liftIO $ getChar
                                     let env' = Map.insert ((Map.size env)+1) (CharFn c) env
                                     let machine' = Machine (code machine) env' (dump machine)
                                     S.put machine'
                              Out -> do
                                       case v of
                                         CharFn cc -> do
                                                       liftIO $ putChar cc
                                                       let env' = Map.insert ((Map.size env)+1) v env
                                                       let machine' = Machine (code machine) env' (dump machine)
                                                       S.put machine'
                                         _ -> return ()
                              Succ -> do
                                       case v of
                                         CharFn cc -> do
                                                       let env' = Map.insert ((Map.size env)+1) (CharFn (chr $ (ord cc)+1)) env
                                                       let machine' = Machine (code machine) env' (dump machine)
                                                       S.put machine'
                                         _ -> return ()
                              _ -> return ()
                        ; return ()
                        }
evalInst (Abs arity insts) = do { machine <- S.get 
                                ; let env = environment machine
                                ; let f = Fn (arity-1) insts env
                                ; let env' = Map.insert ((Map.size env)+1) f env
                                ; let machine' = Machine (code machine) env' (dump machine)
                                ; S.put machine'
                                }


eval :: SECD ()
eval = do
  machine <- S.get
  let c = code machine

  if not (null c)
  then do
    let inst = head c
    let rest = tail c
    liftIO $ printInst inst
    let machine' = Machine rest (environment machine) (dump machine)
    S.put machine'

    liftIO $ printCode $ code machine
    liftIO $ printEnv $ environment machine
    evalInst inst
    m <- S.get
    liftIO $ printCode $ code m
    liftIO $ printEnv $ environment m 
    eval
  else do
    if (null (dump machine))
    then return ()
    else do
      let env = environment machine
      let ret = fromJust $ Map.lookup (Map.size env) env
      let code' = fst $ head (dump machine)
      let env' = snd $ head (dump machine)
      let env'' = Map.insert ((Map.size env')+1) ret env'
      let dump' = tail (dump machine)
      let machine' = Machine code' env'' dump'
      S.put machine'
      eval



absParser :: Stream s m Char => ParsecT s u m Inst
absParser = do { arity_seq <- aritySeqParser
               ; let arity = length arity_seq
               ; insts <- P.many1 appParser
               ; return $ Abs arity insts
               }
  where
    aritySeqParser :: Stream s m Char => ParsecT s u m [Char]
    aritySeqParser = P.many1 (P.char '姐')


appParser :: Stream s m Char => ParsecT s u m Inst
appParser = do { idx_of_function_seq <- idxOfFunctionParser
               ; idx_of_argument_seq <- idxOfArgumentParser
               ; P.spaces
               ; let idx_of_function = length idx_of_function_seq
               ; let idx_of_argument = length idx_of_argument_seq
               ; return $ App idx_of_function idx_of_argument
               }
  where 
    idxOfFunctionParser :: Stream s m Char => ParsecT s u m [Char]
    idxOfFunctionParser = P.many1 (P.char '姊')

    idxOfArgumentParser :: Stream s m Char => ParsecT s u m [Char]
    idxOfArgumentParser = P.many1 (P.char '姐')


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
