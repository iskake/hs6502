{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import HS6502
import HS6502.Debug
import qualified Data.ByteString as B -- TODO? replace with Data.Text
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
      (result,cpustate'') <- runCPU (runOP op) cpustate'

      case result of
          Left a -> do
            B.putStr ("A FATAL ERROR OCCURRED:\n  " <> a <> "\n")
            putStrLn "\nDEBUG: continuing execution at last valid CPU state..."
            putStrLn (printCPUState cpustate)
            putStrLn (printNextInstr cpustate)
            runDebugger cpu
          Right () -> do
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
      keepRunningCPUState cpu  -- runs forever
    ["p"] -> do
      (_,cpustate) <- cpu
      putStrLn (printCPUState cpustate)
      putStrLn (printNextInstr cpustate)
      runDebugger cpu
    ["pi"] -> do
      putStrLn "List of valid instructions (format: `$op: inst`)"
      mapM_ printInstFromOp [0..255]
      putStrLn "Note: `$ff` is used here to represent any 8-bit hexadecimal number, and can be  "
      putStrLn "     written in assembly as any value from $00-$ff (0-255).                     "
      putStrLn "      `$ffff` is any 16-bit hex number, and can be written in assembly as either"
      putStrLn "     any value from $0000-ffff (0-65535) or as any known label                  "
      putStrLn "     (labels do not show up in this debugger, instead their memory address is.) "
      runDebugger cpu
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
  let r = (runCPU (runOP op) cpustate')
  (result,_) <- r
  case result of
      Left a -> do
        B.putStr ("A FATAL ERROR OCCURRED:\n  " <> a <> "\n")
        putStrLn "\nDEBUG: continuing execution at last valid CPU state..."
        putStrLn (printCPUState cpustate)
        putStrLn (printNextInstr cpustate)
        runDebugger cpu
      Right () -> keepRunningCPUState r

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
  putStrLn "  pi - Show a list of all instructions and their coressponding opcode."
  putStrLn "  h  - show this help text."