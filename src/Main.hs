-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Main where

import System.Environment (getArgs)

import Command (add, graph, rebase, remove, status)

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
