-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module RepositoryUtils (withCurrentRepository) where

import Control.Monad.Trans.Reader
import Git
import Git.Libgit2 as G2
import System.Directory (doesDirectoryExist)
import System.Exit (exitFailure)
import System.FilePath.Posix ((</>), takeDirectory)
import System.Posix.Directory (getWorkingDirectory)

-- The Git library is a bit cryptic with its types. If we just ignore that and
-- call the functions, then the withCurrentRepository function will be able to
-- run everything and produce an IO operation.
type GitOperation = ReaderT G2.LgRepo IO

-- If the provided directory contains a .git directory, returns it, otherwise
-- searches at a higher level, until we cannot go up any more.
findGitDirectory :: FilePath -> IO (Maybe FilePath)
findGitDirectory dir | (dir == takeDirectory dir) = return Nothing
findGitDirectory dir | otherwise = do
  containsDotGit <- doesDirectoryExist (dir </> ".git")
  if containsDotGit
    then return (Just dir)
    else findGitDirectory (takeDirectory dir)

makeOptions :: FilePath -> FilePath -> RepositoryOptions
makeOptions workingDir dir = RepositoryOptions {
  repoPath       = dir,
  repoWorkingDir = Just workingDir,
  repoIsBare     = False,
  repoAutoCreate = False
}

-- Returns repository options to open the repository in which the working
-- directory lies.
currentOpenOptions :: IO (Maybe RepositoryOptions)
currentOpenOptions = do
  workingDir  <- getWorkingDirectory
  maybeGitDir <- findGitDirectory workingDir
  return $ fmap (makeOptions workingDir) maybeGitDir

printNotGitRepository :: IO a
printNotGitRepository = do
  putStrLn "fatal: Not a git repository (or any of the parent directories): .git"
  exitFailure

-- Performs the Git operations on the repository in which the working directory
-- lies.
withCurrentRepository :: GitOperation a -> IO a
withCurrentRepository gitOperation = do
  maybeOpenOpts <- currentOpenOptions
  maybe printNotGitRepository performOperation maybeOpenOpts
    where performOperation opts = withRepository' G2.lgFactory opts gitOperation
