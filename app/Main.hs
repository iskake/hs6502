{-# LANGUAGE OverloadedStrings #-}

module Main where

import HS6502
import qualified Data.ByteString as B

main :: IO ()
main = do
  print $ map opToInst [0..255]
  putStrLn "hs6502 debugger"
  -- runDebugger undefined undefined

  -- TEMP 'debugging':
  -- print $ runInst LDA IndX emptyState {rX = 1, rPC = 0, cMem = U8Memory (fromList ([0x01,0x01,0x00,0x01,0x10,0x11] ++ (Prelude.take 512 (repeat 0))))}

runDebugger :: a -> b -> IO ()
runDebugger cpu memory = do
  B.putStr $ " > "
  command <- getLine
  let p = words command
  case p of
    ["r"] -> do
      let cpu' = undefined
      let memory = [0,1,2,3] :: [Register8]
      -- cpu' <- stepCPU cpu
      runDebugger cpu memory
    [x] | x `elem` ["q", "quit", "exit"] -> putStrLn "Bye!"
    _ -> do
      putStrLn "?"  -- just like ed intened
      runDebugger cpu memory
