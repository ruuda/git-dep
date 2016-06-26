-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}

module RepositoryUtils (GitOperationFree) where

import Control.Monad.Free

data GitOperationFree a
  = GetConfig String (String -> a)
  | SetConfig String String a
  | ListBranches ([String] -> a)
  deriving (Functor)

type GitOperation = Free GitOperationFree
