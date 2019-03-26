#!/usr/bin/env ghc
module Main (main) where

import System.Console.GetOpt
import System.Exit
import System.Environment
import Control.Monad
import System.IO
import Data.List
import Data.Char
import Data.Maybe

data Options = Options  { optVerbose    :: Bool
                        , optInput      :: IO String
                        , optOutput     :: String -> IO ()
                        , optProblem    :: Integer
                        }

startOptions :: Options
startOptions = Options  { optVerbose    = False
                        , optInput      = getContents
                        , optOutput     = putStr
                        , optProblem    = -1
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        (ReqArg
            (\arg opt -> return opt { optInput = readFile arg })
            "FILE")
        "Input file"

    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = writeFile arg })
            "FILE")
        "Output file"

    , Option "p" ["problem"]
        (ReqArg
            (\arg opt -> return opt { optProblem = read arg :: Integer })
            "NUMBER")
        "Problem number"

    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"

    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitWith ExitSuccess))
        "Print version"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

getAlgorithm :: (Integral a, Num b) => a -> Maybe b
getAlgorithm 1 = Just $ fromIntegral problem_1
-- getAlgorithm 2 = Just 11.1
getAlgorithm x = Nothing

main = do
    args <- getArgs

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt Permute options args

    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions

    let Options { optVerbose = verbose
                , optInput = input
                , optOutput = output
                , optProblem = problem} = opts

    if problem <= 0 then do
      hPutStrLn stderr "No valid problem number given"
      exitFailure else
      return ()

    let result = getAlgorithm problem

    case result of Nothing -> do
                     hPutStrLn stderr $ "Problem " ++ show problem ++ " not solved yet"
                     exitFailure
                   Just a -> hPutStrLn stdout $ "Solution to problem " ++
                     show problem ++ " is: " ++ show a

    exitSuccess

problem_1 :: Integer
problem_1 = sum [0,3..limit-1] + sum [0,5..limit-1] - sum [0,15..limit-1] where
  limit = 1000
