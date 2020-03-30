{-|
Module      : Prosidy
Description : Top-level module exporting types, parsing functions, and optics.
Copyright   : ©2020 James Alexander Feldman-Crough
License     : MPL-2.0
Maintainer  : alex@fldcr.com
-}
{-# LANGUAGE Safe #-}
module Prosidy (module X) where

import           Prosidy.Source                as X
                                                ( Source
                                                , Location
                                                , Line
                                                , Column
                                                , Offset
                                                )
import           Prosidy.Optics                as X
import           Prosidy.Parse                 as X
import           Prosidy.Types                 as X

import           Prosidy.Internal.JSON          ( )
