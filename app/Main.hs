{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import HS6502
import HS6502.Debug
import Data.Word
import qualified Data.ByteString as B
import Memory
import Data.Bits ((.&.))

import System.Environment
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "hs6502 debugger"
  args <- getArgs
  fileName <- case args of
      [] -> B.putStr "Filename: " >> getLine
      a:_ -> return a
  putStrLn $ "Running file: " <> fileName
  file <- B.readFile fileName
  print $ file
  let x = B.unpack file
  -- y <- initialState
  runDebugger (runCPU (return ()) (emptyState (memCreate x (B.length file))))

runDebugger cpu = do
  B.putStr $ " > "
  command <- getLine
  let cmd = words command
  case cmd of
    ["r"] -> do
      (_,cpustate) <- cpu
      (op, cpustate') <- (return . pcReadInc) cpustate
      (_,cpustate'') <- runCPU (runOP op) cpustate'

      let p = rP cpustate''
      let b = fB p
      if b
        then do
          let cpustate''' =  cpustate'' {rP = (rP cpustate'') {fB = False}}
          putStrLn "BRK called!!!"
          putStrLn (printCPUState cpustate''')
          putStrLn (printNextInstr cpustate''')
          runDebugger (return (Right (), cpustate'''))
        else do
          putStrLn (printCPUState cpustate'')
          putStrLn (printNextInstr cpustate'')
          runDebugger (return (Right (), cpustate''))
    ["c"] -> do
      -- c <- cpu
      !cpu' <- keepRunningCPUState cpu  -- runs forever
      runDebugger cpu'
    ["p"] -> do
      (_,cpustate) <- cpu
      putStrLn (printCPUState cpustate)
      putStrLn (printNextInstr cpustate)
      runDebugger cpu
    ["x"] -> do
      (_,cpustate) <- cpu
      let pc = rPC cpustate
      let pc' = pc .&. 0xfff0
      let f a = putStrLn $ hex16 a ++ ": " ++ concatMap (hex8 . addrRead cpustate) [a..(a+15)]
      f pc'
      f (pc'+16)
      f (pc'+32)
      runDebugger cpu
    "x":_ -> do
      putStrLn "Usage: `x [start end]`"
      runDebugger cpu
    ["d"] -> do
      (_,cpustate) <- cpu
      let pc = (rPC cpustate)
      let mem = (cMem cpustate)
      mapM_ print (disasSect pc (pc+15) mem)
      runDebugger cpu
    ["d", x, y] -> do
      (_,cpustate) <- cpu
      let x' = read x :: Word16
      let y' = read y :: Word16
      mapM_ print (disasSect x' y' (cMem cpustate))
      runDebugger cpu
    ["w", x, y] -> do
      (_,cpustate) <- cpu
      let x' = read x :: Word16
      let y' = read y :: Word16
      mapM_ print (disasSect x' y' (cMem cpustate))
      runDebugger cpu
    [x] | x `elem` ["q", "quit", "exit"] -> putStrLn "Bye!"
    [] -> runDebugger cpu
    _ -> do
      putStrLn "?"  -- just like ed intened
      runDebugger cpu

keepRunningCPUState cpu = do
  (_,cpustate) <- cpu
  let (op, cpustate') = pcReadInc cpustate
  keepRunningCPUState (runCPU (runOP op) cpustate')