-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Git (getDependencies, listBranches) where

import Data.List (stripPrefix)
import Data.Maybe (fromJust)

import GitPlumbing (GitOperation, runGitCommand, tryGitCommand)

-- Some newtypes to make working with Git more type safe.

newtype Sha    = Sha String
newtype Branch = Branch String

-- At some point we need to communicate with the outside world and drop the
-- wrapper types by using show.

instance Show Sha where
  show (Sha sha) = sha

instance Show Branch where
  show (Branch branch) = branch

listBranches :: GitOperation [Branch]
listBranches = do
  branches <- runGitCommand ["for-each-ref", "--format=%(refname)", "refs/heads"]
  return $ fmap parseBranch $ lines branches
    where parseBranch = Branch . fromJust . stripPrefix "refs/heads/"

getDependencies :: Branch -> GitOperation [Branch]
getDependencies branch = do
  deps <- tryGitCommand ["config", "--get", "branch." ++ (show branch) ++ ".deps"]
  -- The "deps" config value contains a space-separated list of branch names. It
  -- can happen that the "deps" key does not exist at all. In that case, return
  -- the empty list.
  case deps of
    Just depsList -> return $ fmap Branch $ words depsList
    Nothing       -> return []
