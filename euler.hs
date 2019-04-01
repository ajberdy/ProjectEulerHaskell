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
import Data.Ord
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
                mapM_ ((hPutStrLn stderr).testProblem) problems
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

testProblem :: Problem -> String
testProblem problem =
  if computed == solution then
    "Problem " ++ show pNumber ++ " is " ++ green ++ "correct" ++ defaultColor
    ++ ": " ++ show computed
    else
    "Problem " ++ show pNumber ++ " is " ++ red ++ "incorrect" ++ defaultColor
    ++ ": " ++ show computed
    ++ " (should be " ++ show solution ++ ")"
  where
    computed = problemAlgorithm problem
    solution = case Map.lookup (problemNumber problem) solutions of
      Nothing -> 0
      Just x -> x
    pNumber = problemNumber problem

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


problem_3 :: Integer
problem_3 = largestFactor 600851475143

goesInto :: Integer -> Integer -> Bool
goesInto n p = n `mod` p == 0

primes :: [Integer]
primes = 2 : (filter isPrime [3,5..])

smallestFactor :: Integer -> Integer
smallestFactor n = head $ filter (goesInto n) [2..bound] ++ [n]
  where bound = floor $ sqrt $ fromIntegral n

isPrime :: Integer -> Bool
isPrime n = primeFactors n == [n]

primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
  where factor n (p:ps)
          | p*p > n      = [n]
          | goesInto n p = p : factor (n `div` p) (p:ps)
          | otherwise    = factor n ps

largestFactor :: Integer -> Integer
largestFactor n = last $ primeFactors n


problem_4 :: Integer
problem_4 = maximum palindromes
  where palindromes = filter isPalindrome [a * b | a <- [100..999], b <- [100..a]]

isPalindrome :: (Show a) => a -> Bool
isPalindrome x = xStr == reverse xStr
  where xStr = show x


problem_5 :: Integer
problem_5 = foldl1 lcm [2..20]


problem_6 :: Integer
problem_6 = (sum [1..100])^2 - (sum $ map (^2) [1..100])


problem_7 :: Integer
problem_7 = primes !! (10001 - 1)


problem_8 :: Integer
problem_8 = maximum $ map thirteenProduct [0..(length problem8Input) - 13]
  where thirteenProduct ix = toInteger $ product $ map extract [ix..ix + 12]
        extract idx = digitToInt $ problem8Input !! idx


problem_9 :: Integer
problem_9 = a' * b' * c'
  where (a', b', c') = head [(a, b, c) |
                             a <- [1..1000], b <- [1..a], c <- [1000 - a - b],
                             a^2 + b^2 == c^2]


problem_10 :: Integer
problem_10 = sum $ takeWhile (< 2000000) primes


problem_11 :: Integer
problem_11 = maximum $ map getProduct indices
  where
    indices = foldl1 union [ [[(a, b), (a + 1, b), (a + 2, b), (a + 3, b)] |
                              a <- [0..16], b <- [0..19]]
                           , [[(a, b), (a, b + 1), (a, b + 2), (a, b + 3)] |
                              a <- [0..19], b <- [0..16]]
                           , [[(a, b), (a + 1, b + 1), (a + 2, b + 2), (a + 3, b + 3)] |
                              a <- [0..16], b <- [0..16]]
                           , [[(a, b + 3), (a + 1, b + 2), (a + 2, b + 1), (a + 3, b)] |
                              a <- [0..16], b <- [0..16]]
                           ]
    getProduct ixList = product $ map extract ixList
    extract (i, j) = (read::String->Integer) $ (words problem11Input) !! (20 * i + j)


problem_12 :: Integer
problem_12 = head $ filter highlyDivisible triangles
  where
    highlyDivisible n = (numFactors n) > 500
    triangles = map triangle [1..]
      where
        triangle n = (n^2 + n) `div` 2

primeFactorsMap :: Integer -> Map.Map Integer Integer
primeFactorsMap n =
  Map.fromListWith (+) (zip factors [1,1..])
  where
    factors = primeFactors n

numFactors :: Integer -> Integer
numFactors n =
  product $ map (+1) (Map.elems $ primeFactorsMap n)

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n - 1)

factors :: Integer -> [Integer]
factors n = filter (goesInto n) [1..n]


problem_13 :: Integer
problem_13 = (read::String->Integer) $ take 10 sumString
  where
    sumString = show $ sum $ map (read::String->Integer) $ words problem13Input

problem_14 :: Integer
problem_14 = snd . foldl1 max $ map (\x -> (collatzLength x, x)) [1..1000000]

memoize :: (Integer -> a) -> (Integer -> a)
memoize f = (map (f . toInteger) [0..] !!) . fromIntegral

collatzLength :: Integer -> Integer
collatzLength =
  toInteger . length . takeWhile (/= 1) . iterate collatzStep

collatzStep :: Integer -> Integer
collatzStep x
  | even x    = x `div` 2
  | otherwise = 3*x + 1


problem_15 :: Integer
problem_15 = binom 40 20

binom :: Integer -> Integer -> Integer
binom n k = product [n - k + 1 .. n] `div` product [1..k]


problem_16 :: Integer
problem_16 = toInteger . sum . map digitToInt $ show $ 2 ^ 1000

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
           , Problem { problemName      = "Largest palindrome product"
                     , problemNumber    = 4
                     , problemAlgorithm = problem_4
                     }
           , Problem { problemName      = "Even Fibonacci numbers"
                     , problemNumber    = 5
                     , problemAlgorithm = problem_5
                     }
           , Problem { problemName      = "Smallest multiple"
                     , problemNumber    = 6
                     , problemAlgorithm = problem_6
                     }
           , Problem { problemName      = "Sum square difference"
                     , problemNumber    = 7
                     , problemAlgorithm = problem_7
                     }
           , Problem { problemName      = "Largest product in a series"
                     , problemNumber    = 8
                     , problemAlgorithm = problem_8
                     }
           , Problem { problemName      = "Special Pythagorean triplet"
                     , problemNumber    = 9
                     , problemAlgorithm = problem_9
                     }
           , Problem { problemName      = "Summation of primes"
                     , problemNumber    = 10
                     , problemAlgorithm = problem_10
                     }
           , Problem { problemName      = "Largest product in a grid"
                     , problemNumber    = 11
                     , problemAlgorithm = problem_11
                     }
           , Problem { problemName      = "Highly divisible triangular number"
                     , problemNumber    = 12
                     , problemAlgorithm = problem_12
                     }
           , Problem { problemName      = "Large sum"
                     , problemNumber    = 13
                     , problemAlgorithm = problem_13
                     }
           , Problem { problemName      = "Longest Collatz sequence"
                     , problemNumber    = 14
                     , problemAlgorithm = problem_14
                     }
           , Problem { problemName      = "Lattice paths"
                     , problemNumber    = 15
                     , problemAlgorithm = problem_15
                     }
           , Problem { problemName      = "Power digit sum"
                     , problemNumber    = 16
                     , problemAlgorithm = problem_16
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
                         , (9, 31875000)
                         , (10, 142913828922)
                         , (11, 70600674)
                         , (12, 76576500)
                         , (13, 5537376230)
                         , (14, 837799)
                         , (15, 137846528820)
                         , (16, 1366)
                         ]

--  LocalWords:  fibMemo
