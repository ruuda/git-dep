-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then printUsage
    else handleCommand (head args) (tail args)

handleCommand :: String -> [String] -> IO ()
handleCommand "add"    = add
handleCommand "remove" = remove
handleCommand "rebase" = rebase
handleCommand "status" = status
handleCommand "graph"  = graph
handleCommand "--help" = const help
handleCommand "-h"     = const printUsage
handleCommand cmd      = const (printUnknownCommand cmd)

add :: [String] -> IO ()
add args = undefined

remove :: [String] -> IO ()
remove args = undefined

rebase :: [String] -> IO ()
rebase args = undefined

status :: [String] -> IO ()
status args = undefined

graph :: [String] -> IO ()
graph args = undefined

help :: IO ()
help = undefined

printUsage :: IO ()
printUsage = do
  putStrLn "usage: git dep add <dependency> [<branch>]"
  putStrLn "       git dep remove <dependency> [<branch>]"
  putStrLn "       git dep rebase [<branch>]"
  putStrLn "       git dep status [<branch>]"
  putStrLn "       git dep graph [<branch>]"

printUnknownCommand :: String -> IO ()
printUnknownCommand cmd = putStrLn $
  "git dep: '" ++ cmd ++ "' is not a git dep command. See 'git dep --help'."
