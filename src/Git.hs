-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Git (listBranches) where

import Data.Char (isSpace)
import Data.List (stripPrefix)
import Data.Maybe (fromJust)

import GitPlumbing (GitOperation, runGitCommand)

-- Parses the output of `git for-each-ref refs/heads`.
parseBranches :: String -> [String]
parseBranches raw = map takeBranch $ lines raw
  where takeBranch line =
          let strippedSha  = tail $ dropWhile (not . isSpace) line
              strippedType = tail $ dropWhile (not . isSpace) strippedSha
              strippedRef  = stripPrefix "refs/heads/" strippedType
          in  fromJust strippedRef

listBranches :: GitOperation [String]
listBranches = do
  branches <- runGitCommand ["for-each-ref", "refs/heads"]
  return $ parseBranches branches
