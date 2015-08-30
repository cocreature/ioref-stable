-- |
-- Module      :  Data.IORefStable
-- Copyright   :  (c) Moritz Kiefer 2015
-- License     :  MIT
-- Maintainer  :  moritz.kiefer@purelyfunctional.org
-- The IORefStable type

module Data.IORefStable
  (IORefStable(..)
  ,newIORefStable
  ,readIORefStable
  ,writeIORefStable
  ,modifyIORefStable
  ,modifyIORefStable'
  ,atomicModifyIORefStable
  ,atomicModifyIORefStable'
  ,atomicWriteIORefStable
  ) where

import Data.Unique
import Data.IORef

-- |A mutable variable in the 'IO' monad indexed by a 'Unique' to
-- provide an Ord instance
data IORefStable a = IORefStable !Unique !(IORef a) deriving (Eq)

instance Ord (IORefStable a) where
  (IORefStable u1 _) `compare` (IORefStable u2 _) = u1 `compare` u2

-- |Build a new 'IORefStable'
newIORefStable :: a -> IO (IORefStable a)
newIORefStable a = do
  u <- newUnique
  ref <- newIORef a
  return (IORefStable u ref)

-- |Read the value of an 'IORefStable'
readIORefStable :: IORefStable a -> IO a
readIORefStable (IORefStable _ r) = readIORef r

-- |Write a new value into an 'IORefStable'
writeIORefStable :: IORefStable a -> a -> IO ()
writeIORefStable (IORefStable _ r) a = writeIORef r a

-- |Mutate the contents of an 'IORefStable'
modifyIORefStable :: IORefStable a -> (a -> a) -> IO ()
modifyIORefStable (IORefStable _ r) f = modifyIORef r f

-- |Strict version of 'modifyIORefStable'
modifyIORefStable' :: IORefStable a -> (a -> a) -> IO ()
modifyIORefStable' (IORefStable _ r) f = modifyIORef' r f

-- |Atomically modifies the contents of an 'IORefStable'
atomicModifyIORefStable :: IORefStable a -> (a -> (a, b)) -> IO b
atomicModifyIORefStable (IORefStable _ r) f = atomicModifyIORef r f

-- |Strict version of 'atomicModifyIORefStable'
atomicModifyIORefStable' :: IORefStable a -> (a -> (a, b)) -> IO b
atomicModifyIORefStable' (IORefStable _ r) f = atomicModifyIORef' r f

-- |Atomically write a new value into an 'IORefStable'
atomicWriteIORefStable :: IORefStable a -> a -> IO ()
atomicWriteIORefStable (IORefStable _ r) a = atomicWriteIORef r a
