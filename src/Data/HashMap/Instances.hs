{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.HashMap.Instances
-- License     :  BSD3
-- Maintainer  :  zcarterc@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Orphan Binary instance for HashMap

module Data.HashMap.Instances where

import Control.Applicative
import Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.Binary

instance (Eq k, Hashable k, Binary k, Binary v) => Binary (HashMap.HashMap k v) where
  put x = put (HashMap.toList x)
  get = HashMap.fromList <$> get
