module Debugger (
    maybeTrace,
    traceThis
  ) where

import Debug.Trace


maybeTrace :: (Show a) => Maybe a -> Maybe a
maybeTrace Nothing = Nothing
maybeTrace x = traceThis x


traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x
