-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}

module GitPlumbing
(
  GitOperation,
  liftIO,
  runGitCommand,
  runGit
)
where

import Control.Monad.Free
import System.Exit
import System.IO
import System.Process

-- Define a data type for the free monad. These represent the "low level" Git
-- commands that can be invoked. There are just two of these: call git, or do a
-- different IO operation.
data GitOperationFree a
  = RunGitCommand [String] (String -> a)
  | LiftIO (IO ()) (() -> a) -- TODO: Can I make it generic without resorting to GADTs?
  deriving (Functor)

type GitOperation = Free GitOperationFree

-- Every Git operation gets a corresponding function. These functions may be
-- thought of as directly performing their operation, but in fact all they do is
-- build up a sequence of commands that is later interpreted by runGit.

-- Calls Git with the given arguments and returns its output.
runGitCommand :: [String] -> GitOperation String
runGitCommand args = liftF $ RunGitCommand args id

-- Allows mixing IO operations with Git operations.
liftIO :: IO () -> GitOperation ()
liftIO action = liftF $ LiftIO action id

-- The runGit function is an interpreter for the GitOperation free monad. It
-- accepts a command and invokes Git. It also takes care of error handling: if a
-- command fails, its output will be redirected to stderr, and subsequent
-- commands will not be executed.
runGit :: GitOperation a -> IO a
runGit op = case op of
  (Pure a)                      -> return a
  (Free (LiftIO action h))      -> action >>= runGit . h
  (Free (RunGitCommand args h)) -> callGit args >>= handleResult
    where handleResult (Just output) = return output >>= runGit . h
          handleResult Nothing       = undefined -- TODO: Use System.Exit here.

callGit :: [String] -> IO (Maybe String)
callGit args = do
  (exitCode, output, errors) <- readProcessWithExitCode "git" args ""
  if exitCode == ExitSuccess
    then return (Just output)
    else failWith errors

failWith :: String -> IO (Maybe String)
failWith message = do
  hPutStr stderr message
  return Nothing
  -- TODO: Probably keep the exit code, and re-use it later.
