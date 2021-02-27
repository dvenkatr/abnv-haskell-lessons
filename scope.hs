-- print3broken.hs

module Print3Broken where

greeting = "yes"

printSecond :: IO ()
printSecond = do
  putStrLn greeting
  -- where greeting = "Yarrrrr"

main :: IO ()
main = do
  putStrLn greeting
  printSecond
