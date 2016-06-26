-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}

module RepositoryUtils
(
  GitOperation,
  getConfig,
  setConfig,
  listBranches,
  liftIO,
  runGit
)
where

import Control.Monad.Free

-- Define a data type for the free monad. These represent the "low level" Git
-- commands that can be invoked.
data GitOperationFree a
  = GetConfig String (String -> a)
  | SetConfig String String a
  | ListBranches ([String] -> a)
  | LiftIO (IO ()) (() -> a) -- TODO: Can I make it generic without resorting to GADTs?
  deriving (Functor)

type GitOperation = Free GitOperationFree

-- Every Git operation gets a corresponding function. These functions may be
-- thought of as directly performing their operation, but in fact all they do is
-- build up a sequence of commands that is later interpreted by runGit.

getConfig :: String -> GitOperation String
getConfig key = liftF $ GetConfig key id

setConfig :: String -> String -> GitOperation ()
setConfig key value = liftF $ SetConfig key value ()

listBranches :: GitOperation [String]
listBranches = liftF $ ListBranches id

-- Allows mixing IO operations with Git operations.
liftIO :: IO () -> GitOperation ()
liftIO action = liftF $ LiftIO action id

-- The runGit function is an interpreter for the GitOperation free monad. It
-- accepts a command and invokes Git. It also takes care of error handling: if a
-- command fails, its output will be redirected to stderr, and subsequent
-- commands will not be executed.

runGit :: GitOperation a -> IO a

runGit (Pure a) = return a

runGit (Free (GetConfig key h)) = do
  putStrLn $ "not implemented: get config " ++ key
  return "null" >>= runGit . h

runGit (Free (SetConfig key value a)) = do
  putStrLn $ "not implemented: set config " ++ key ++ " = " ++ value
  return a >>= runGit

runGit (Free (ListBranches h)) = do
  putStrLn "not implemented: list branches"
  return [] >>= runGit . h

runGit (Free (LiftIO action h)) = action >>= runGit . h
