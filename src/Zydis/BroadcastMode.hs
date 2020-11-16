-- BroadcastMode.hs ---

-- Copyright (C) 2020 Nerd Ed

-- Author: Nerd Ed <nerded.nerded@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DerivingVia #-}

module Zydis.BroadcastMode
  ( BroadcastMode(..)
  )
where

import           Zydis.Util

data BroadcastMode
  = BroadcastModeInvalid
  | BroadcastMode1To2
  | BroadcastMode1To4
  | BroadcastMode1To8
  | BroadcastMode1To16
  | BroadcastMode1To32
  | BroadcastMode1To64
  | BroadcastMode2To4
  | BroadcastMode2To8
  | BroadcastMode2To16
  | BroadcastMode4To8
  | BroadcastMode4To16
  | BroadcastMode8To16
  deriving stock (Show, Eq, Bounded, Enum)
  deriving Storable via StorableExt BroadcastMode
