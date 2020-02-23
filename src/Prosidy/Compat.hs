{-# LANGUAGE CPP #-}
module Prosidy.Compat 
  ( MonadFail(..) 
  ) where

#if __GLASGOW_HASKELL__ < 808
import Control.Monad.Fail (MonadFail(..))
#endif