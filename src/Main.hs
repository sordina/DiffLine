{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.InterpolatedString.Perl6 -- For tests
import System.Environment (getArgs)
import Data.List.Split    (splitOn)
import Data.List          (intercalate)
import Data.Maybe         (isNothing)

-- IO:

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run args | null args                                     = runFun onSlash
         | any ((== args) . return) ["-s", "--slashes" ] = runFun onSlash
         | any ((== args) . return) ["-r", "--repeats" ] = runFun onRepeat
         | any ((== args) . return) ["-v", "--vertical"] = runFun onVertical
         | any (`elem` args)        ["-h", "--help"    ] = help
         | otherwise                                     = help

runFun :: ([String] -> [String]) -> IO ()
runFun f = getContents >>= putStr . unlines . f . lines

help :: IO ()
help = putStrLn "Usage: diffline [-s | --slashes (default) ] [-r | --repeats] [-v | --vertical] [-h | --help]"

-- Algorithm:

onRepeat :: [String] -> [String]
onRepeat = removeCommonPrefixes (const ' ')

onVertical :: [String] -> [String]
onVertical  = map (intercalate "/") . removeCommonPrefixes (map (const ' ')) . map (splitOn "/")

onSlash :: [String] -> [String]
onSlash  = map concat . removeCommonPrefixes (map (const ' ')) . map deslash

deslash :: String -> [String]
deslash l = take 1 items ++ map ('/' :) (drop 1 items) where items = splitOn "/" l

removeCommonPrefixes :: Eq a => (a -> a) -> [[a]] -> [[a]]
removeCommonPrefixes f l = zipWith (prefixPair f) ([] : l) l

prefixPair :: Eq a => (a -> a) -> [a] -> [a] -> [a]
prefixPair f a b = pre ++ post
  where
    eqs  = zipWith (==) (map Just a ++ repeat Nothing) (map Just b)
    x    = zip eqs b
    pre  = map (f . snd) $ takeWhile fst x
    post = map snd       $ dropWhile fst x

-- TESTS:

prop_first_is_different :: Eq a => [a] -> [a] -> Bool
prop_first_is_different a b = let (a', b') = (map Just a, map Just b)
                              in all (uncurry (/=))
                               $ take 1 $ dropWhile  (isNothing . snd)
                               $ zip a' $ prefixPair (const Nothing) a' b'

prop_indempotent_repeat, prop_indempotent_slash, prop_indempotent_vertical :: [String] -> Bool

prop_indempotent_repeat   ls = onRepeat   ls == onRepeat   (onRepeat   ls)
prop_indempotent_slash    ls = onSlash    ls == onSlash    (onSlash    ls)
prop_indempotent_vertical ls = onVertical ls == onVertical (onVertical ls)

prop_slash, prop_repeat, prop_vertical :: Bool

prop_repeat   = testOutputRepeat   == onRepeat   testInput
prop_slash    = testOutputSlash    == onSlash    testInput
prop_vertical = testOutputVertical == onVertical testInput

testInput :: [String]
testInput = lines [qq|
temp
temp/-name
temp/-name/serf
temp/-name/serf/local.snapshot
temp/-node
temp/-node/serf
temp/-node/serf/local.snapshot
temp/.DS_Store
temp/a
temp/a.js
|]

testOutputRepeat :: [String]
testOutputRepeat = lines [qq|
temp
    /-name
          /serf
               /local.snapshot
       ode
          /serf
               /local.snapshot
     .DS_Store
     a
      .js
|]

testOutputSlash :: [String]
testOutputSlash = lines [qq|
temp
    /-name
          /serf
               /local.snapshot
    /-node
          /serf
               /local.snapshot
    /.DS_Store
    /a
    /a.js
|]

testOutputVertical :: [String]
testOutputVertical = lines [qq|
temp
    /-name
    /     /serf
    /     /    /local.snapshot
    /-node
    /     /serf
    /     /    /local.snapshot
    /.DS_Store
    /a
    /a.js
|]
