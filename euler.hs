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
import qualified Data.Map as Map

import Constants

data Options = Options  { optProblem    :: Integer }

startOptions :: Options
startOptions = Options  { optProblem    = -1 }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "p" ["problem"]
        (ReqArg
            (\arg opt -> return opt { optProblem = read arg :: Integer })
            "NUMBER")
        "Problem number"

    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"

    , Option "t" ["test"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Testing Solutions"
                let computed = map problemAlgorithm problems
                let incorrect = filter (not.correct) problems
                      where correct p = computed == solution
                              where computed = problemAlgorithm p
                                    solution = case Map.lookup (problemNumber p) solutions of
                                      Nothing -> 0
                                      Just x -> x
                if not $ null incorrect then
                  hPutStrLn stderr "Tests Failed"
                  else
                  hPutStrLn stderr "Tests Passed"
                exitWith ExitSuccess))
        "Test solutions"
    ]

data Problem = Problem { problemName       :: String
                       , problemNumber     :: Integer
                       , problemAlgorithm  :: Integer
                       }

problemMap :: Map.Map Integer Problem
problemMap = Map.fromList $ keyed
  where keyed = zip (map problemNumber problems) problems

main = do
    args <- getArgs

    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt Permute options args

    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions

    let Options { optProblem = problemNumber} = opts

    if problemNumber <= 0 then do
      hPutStrLn stderr "No valid problem number given"
      exitFailure
      else return ()

    let problem = Map.lookup problemNumber problemMap

    case problem of Nothing -> do
                      hPutStrLn stderr $ "Problem " ++ show problemNumber ++ " not solved yet"
                      exitFailure
                    Just p -> do
                      let result = problemAlgorithm p
                      hPutStrLn stdout $ "Solution to problem " ++
                        show problemNumber ++ " is: " ++ show result

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

goesInto :: Integer -> Integer -> Bool
goesInto n p = n `mod` p == 0

smallestFactor :: Integer -> Integer
smallestFactor n = head $ filter (goesInto n) [2..bound] ++ [n]
  where bound = floor $ sqrt $ fromIntegral n

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

problem_5 :: Integer
problem_5 = foldl1 lcm [2..20]

problem_6 :: Integer
problem_6 = (sum [1..100])^2 - foldl addSquare 0 [1..100]
  where addSquare x y = x + y^2

isPrime :: Integer -> Bool
isPrime n = n == smallestFactor n

problem_7 :: Integer
problem_7 = primes !! (10001 - 1)
  where primes = filter isPrime [2..]

problem_8 :: Integer
problem_8 = maximum $ map thirteenProduct [0..(length problem8Input) - 13]
  where thirteenProduct ix = toInteger $ product $ map extract [ix..ix + 12]
        extract idx = digitToInt $ problem8Input !! idx


problems :: [Problem]
problems = [ Problem { problemName      = "Multiples of 3 and 5"
                     , problemNumber    = 1
                     , problemAlgorithm = problem_1
                     }
           , Problem { problemName      = "Even Fibonacci numbers"
                     , problemNumber    = 2
                     , problemAlgorithm = problem_2
                     }
           , Problem { problemName      = "Largest prime factor"
                     , problemNumber    = 3
                     , problemAlgorithm = problem_3
                     }
           , Problem { problemName      = ""
                     , problemNumber    = 4
                     , problemAlgorithm = problem_4
                     }
           , Problem { problemName      = "Even Fibonacci numbers"
                     , problemNumber    = 5
                     , problemAlgorithm = problem_5
                     }
           , Problem { problemName      = "Even Fibonacci numbers"
                     , problemNumber    = 6
                     , problemAlgorithm = problem_6
                     }
           , Problem { problemName      = "Even Fibonacci numbers"
                     , problemNumber    = 7
                     , problemAlgorithm = problem_7
                     }
           , Problem { problemName      = "Largest product in a series"
                     , problemNumber    = 8
                     , problemAlgorithm = problem_8
                     }
           ]

solutions :: Map.Map Integer Integer
solutions = Map.fromList [ (1, 233168)
                         , (2, 4613732)
                         , (3, 6857)
                         , (4, 906609)
                         , (5, 232792560)
                         , (6, 25164150)
                         , (7, 104743)
                         , (8, 23514624000)
                         ]
