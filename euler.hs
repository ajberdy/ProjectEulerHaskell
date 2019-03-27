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

import Constants

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
getAlgorithm 2 = Just $ fromIntegral problem_2
getAlgorithm 3 = Just $ fromIntegral problem_3
getAlgorithm 4 = Just $ fromIntegral problem_4
getAlgorithm 5 = Just $ fromIntegral problem_5
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
problem_1 = sum3And5Multiples 1000

sum3And5Multiples :: Integer -> Integer
sum3And5Multiples limit =
  sum [0,3..limit-1] +  sum [0,5..limit-1] - sum [0,15..limit-1]

problem_2 :: Integer
problem_2 = evenFibSum 4000000

evenFibSum :: Integer -> Integer
evenFibSum limit = sum $ map nthFib [3,6..limitIndex] where
  limitIndex = fibIndex $ fromIntegral limit

nthFib :: Integer -> Integer
nthFib n = floor $ phi^n / sqrt 5 + 1/2

fibIndex :: Double -> Integer
fibIndex x = floor $ logBase phi $ x * sqrt 5 + 1/2

ackerman :: Integer -> Integer -> Integer
ackerman 0 y = y + 1
ackerman x 0 = ackerman (x - 1) 1
ackerman x y = ackerman (x - 1) $ ackerman x (y - 1)

goesInto :: Integer -> Integer -> Bool
goesInto n p = n `mod` p == 0

smallestFactor :: Integer -> Integer
smallestFactor n = head $ filter (goesInto n) [2..]

primeFactorizationList :: Integer -> [Integer]
primeFactorizationList n
  | p == n    = [n]
  | otherwise = p : (primeFactorizationList $ n `div` p)
  where p = smallestFactor n

largestFactor :: Integer -> Integer
largestFactor n = last $ primeFactorizationList n

problem_3 :: Integer
problem_3 = largestFactor 600851475143

isPalindrome :: (Show a) => a -> Bool
isPalindrome x = xStr == reverse xStr
  where xStr = show x

problem_4 :: Integer
problem_4 = maximum palindromes
  where palindromes = filter isPalindrome [a * b | a <- [100..999], b <- [100..a]]

goesIntoPower :: Integer -> Integer -> Integer
goesIntoPower n x =
  if goesInto n x then
    1 + goesIntoPower (n `div` x) x
  else 0

primeFactorization :: Integer -> [(Integer, Integer)]
primeFactorization n
  | n == 1    = []
  | p == n    = [(n, 1)]
  | otherwise = (p, highestPower) : (primeFactorization $ n `div` (p^highestPower))
  where p = smallestFactor n
        highestPower = goesIntoPower n p

leastCommonMultiple :: Integer -> Integer -> Integer
leastCommonMultiple a b =
  foldl factorIn 1 lCMFactors
  where
    factorIn x (p, power) = x * p^power
    lCMFactors = findLCMFactors (primeFactorization a) (primeFactorization b)

findLCMFactors :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
findLCMFactors aFactors [] = aFactors
findLCMFactors [] bFactors = bFactors
findLCMFactors ((aFactor, aPower):aRest) ((bFactor, bPower):bRest)
  | aFactor < bFactor =
    (aFactor, aPower) : (findLCMFactors aRest ((bFactor, bPower):bRest))
  | aFactor == bFactor =
    (aFactor, max aPower bPower) : (findLCMFactors aRest bRest)
  | otherwise =
    (bFactor, bPower) : (findLCMFactors ((aFactor, aPower):aRest) bRest)

problem_5 :: Integer
problem_5 = foldl1 leastCommonMultiple [2..20]

