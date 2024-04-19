{-# LANGUAGE OverloadedStrings #-}

module Main where

import HS6502
import qualified Data.ByteString as B

main :: IO ()
main = do
  print $ map opToInst [0..255]
  -- putStrLn "hs6502 debugger"
  -- runDebugger undefined undefined

runDebugger :: a -> b -> IO ()
runDebugger cpu memory = do
  B.putStr $ " > "
  command <- getLine
  let p = words command
  case p of
    ["r"] -> do
      let cpu = undefined
      let memory = [0,1,2,3] :: [Register8]
      -- cpu' <- stepCPU cpu
      main --runDebugger cpu' memory
    [x] | x `elem` ["q", "quit", "exit"] -> putStrLn "Bye!"
    _ -> do
      putStrLn "?"  -- just like ed intened
      runDebugger cpu memory
