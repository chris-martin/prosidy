{- |
Module      : Prosidy.Internal.Classes
Description : An internal module exporting common classes.
Copyright   : (c) James Alexander Feldman-Crough, 2019
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE Trustworthy #-}
module Prosidy.Internal.Classes (module X) where

import           Data.Aeson                    as X
                                                ( ToJSON(..)
                                                , FromJSON(..)
                                                , ToJSONKey(..)
                                                , FromJSONKey(..)
                                                )
import           Data.Binary                   as X
                                                ( Binary(..) )
import           GHC.Generics                  as X
                                                ( Generic )
import           Control.DeepSeq               as X
                                                ( NFData(..) )
import           Data.Hashable                 as X
                                                ( Hashable(..) )
import           Data.Text.Prettyprint.Doc     as X
                                                ( Pretty(..) )
