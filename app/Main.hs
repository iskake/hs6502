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
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  fileName <- case args of
      [] -> B.putStr "Filename: " >> getLine
      a:_ -> return a
  putStrLn $ "Running file: " <> fileName
  file <- B.readFile fileName
  -- print $ file
  let x = B.unpack file
  -- y <- initialState

  putStrLn "\nhs6502 debugger"
  putStrLn "Type `h` for help."
  runDebugger (runCPU (return ()) (emptyState (memCreate x (B.length file))))

runDebugger cpu = do
  B.putStr $ " > "
  command <- getLine
  let cmd = words command
  case cmd of
    ["s"] -> do
      (_,cpustate) <- cpu
      (op, cpustate') <- (return . pcReadInc) cpustate
      (_,cpustate'') <- runCPU (runOP op) cpustate'

      let p = rP cpustate''
      let b = fB p
      if b
        then do
          let cpustate''' =  cpustate'' {rP = (rP cpustate'') {fB = False}}
          putStrLn "BRK called!!! (currently does nothing...)"
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
    -- ["pi"] -> do
    --   mapM_ 
    --   runDebugger cpu
    ["x"] -> do
      (_,cpustate) <- cpu
      let pc = rPC cpustate
      let pc' = pc .&. 0xfff0
      let f a = putStrLn $ hex16 a <> ": " <> concatMap (hex8 . addrRead cpustate) [a..(a+15)]
      f pc'
      f (pc'+16)
      f (pc'+32)
      runDebugger cpu
    ["x", x] -> do
      (_,cpustate) <- cpu
      case readMaybe x of
        Just x' -> do
          let f a = putStrLn $ hex16 a <> ": " <> concatMap (hex8 . addrRead cpustate) [a..(a+15)]
          f x'
          f (x'+16)
          f (x'+32)
        _ -> putStrLn "x - Examine memory address\nUsage: `x [addr]`"
      runDebugger cpu
    "x":_ -> do
      putStrLn "x - Examine memory address\nUsage: `x [addr]`"
      runDebugger cpu
    ["d"] -> do
      (_,cpustate) <- cpu
      let pc = (rPC cpustate)
      let mem = (cMem cpustate)
      mapM_ print (disasSect pc (pc+15) mem)
      runDebugger cpu
    ["d", x, y] -> do
      (_,cpustate) <- cpu
      case (readMaybe x, readMaybe y) of
          (Just x', Just y') -> mapM_ print (disasSect x' y' (cMem cpustate))
          _ -> putStrLn "d - Disassemble section.\nUsage: `d fromAddr toAddr`"
      runDebugger cpu
    ["w", x, y] -> do
      (_,cpustate) <- cpu
      case (readMaybe x, readMaybe y) of
          (Just val, Just addr) -> runDebugger (return (Right (), addrWrite cpustate addr val)) -- weirdly written...
          _ -> putStrLn "w - Write a value to memory.\nUsage: `w value address`" >> runDebugger cpu
    [x] | x `elem` ["h", "help"] -> printHelp >> runDebugger cpu
    [x] | x `elem` ["q", "quit", "exit"] -> putStrLn "Bye!"
    [] -> runDebugger cpu
    _ -> do
      putStrLn "?"  -- just like ed intened
      runDebugger cpu

keepRunningCPUState cpu = do
  (_,cpustate) <- cpu
  let (op, cpustate') = pcReadInc cpustate
  keepRunningCPUState (runCPU (runOP op) cpustate')

printHelp :: IO ()
printHelp = do
  putStrLn "This is the hs6502 debugger. It can be used to inspect and run through binary programs assembled from the 6502 assembly language."
  putStrLn "Available commands:"
  putStrLn "  s  - step through the program one instruction at a time."
  putStrLn "  c  - Continue running the CPU forever, or until the program crashes."
  putStrLn "  x  - eXamine the memory at a specified memory address."
  putStrLn "  d  - Disassemble the memory at the specified memory address, or (if given no argument,) at the program counter."
  putStrLn "  w  - Write the specified byte to the specified memory address."
  putStrLn "  p  - Print the current CPU state, including register values and the next instruction to be run."
  -- putStrLn "  pi - Show a list of all instructions and their coressponding opcode."
  putStrLn "  h  - show this help text."