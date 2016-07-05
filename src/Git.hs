-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Git
(
  Branch (..),
  addDependency,
  getCurrentBranch,
  getDependencies,
  getTransitiveDependencies,
  listBranches,
  removeDependency
)
where

import           Control.Monad (forM)
import           Data.Char (isSpace)
import           Data.List (delete, intersperse, stripPrefix)
import           Data.Maybe (fromJust)
import           Data.Set (Set)
import qualified Data.Set as Set

import GitPlumbing (GitOperation, runGitCommand, tryGitCommand)

-- Some newtypes to make working with Git more type safe.

newtype Sha    = Sha String deriving (Eq, Ord)
newtype Branch = Branch String deriving (Eq, Ord)

-- At some point we need to communicate with the outside world and drop the
-- wrapper types by using show.

instance Show Sha where
  show (Sha sha) = sha

instance Show Branch where
  show (Branch branch) = branch

-- Adds the dependency to the dependencies of the given branch.
addDependency :: Branch -> Branch -> GitOperation ()
addDependency dependency branch = do
  deps <- getDependencies branch
  -- TODO: Sort and make unique?
  setDependencies branch $ deps ++ [dependency]

listBranches :: GitOperation [Branch]
listBranches = do
  branches <- runGitCommand ["for-each-ref", "--format=%(refname)", "refs/heads"]
  return $ fmap parseBranch $ lines branches
    where parseBranch = Branch . fromJust . stripPrefix "refs/heads/"

getCurrentBranch :: GitOperation Branch
getCurrentBranch = do
  ref <- runGitCommand ["symbolic-ref", "HEAD"]
  -- TODO: Deal with the case where HEAD is not a branch.
  -- TODO: Drop the newline at the end in a stricter way.
  return $ Branch $ fromJust $ stripPrefix "refs/heads/" $ filter (not . isSpace) ref

getDependencies :: Branch -> GitOperation [Branch]
getDependencies branch = do
  deps <- tryGitCommand ["config", "--get", "branch." ++ (show branch) ++ ".deps"]
  -- The "deps" config value contains a space-separated list of branch names. It
  -- can happen that the "deps" key does not exist at all. In that case, return
  -- the empty list.
  case deps of
    Just depsList -> return $ fmap Branch $ words depsList
    Nothing       -> return []

-- Given a branch to start the search, returns a set of dependencies, where the
-- first branch depends on the second one.
getTransitiveDependencies :: Branch -> GitOperation (Set (Branch, Branch))
getTransitiveDependencies rootBranch = aux (Set.singleton rootBranch) Set.empty Set.empty
  where
    makeEdge from to = (from, to)
    makeEdges branch = fmap (Set.fromList . fmap (makeEdge branch)) $ getDependencies branch
    aux new _   deps | Set.null new = return deps
    aux new old deps | otherwise    = do
      edgeSets <- forM (Set.toList new) makeEdges
      let newBranches = Set.fromList $ fmap snd $ concatMap Set.toList edgeSets
          deps'       = Set.unions (deps : edgeSets)
          old'        = Set.union old new
          new'        = Set.difference newBranches old'
      aux new' old' deps'

-- Removes the dependency form the dependencies of the given branch.
removeDependency :: Branch -> Branch -> GitOperation ()
removeDependency dependency branch = do
  deps <- getDependencies branch
  setDependencies branch $ delete dependency deps

-- Sets the dependencies of the given branch.
setDependencies :: Branch -> [Branch] -> GitOperation ()
setDependencies branch deps = do
  -- TODO: Unset key if the list is empty?
  let depsList = concat $ intersperse " " $ fmap show deps
  _ <- runGitCommand ["config", "branch." ++ (show branch) ++ ".deps", depsList]
  return ()
