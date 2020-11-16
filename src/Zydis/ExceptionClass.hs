-- ExceptionClass.hs ---

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

module Zydis.ExceptionClass
  ( ExceptionClass(..)
  )
where

import           Zydis.Util

data ExceptionClass
  = ExceptionClassNone
  | ExceptionClassSse1
  | ExceptionClassSse2
  | ExceptionClassSse3
  | ExceptionClassSse4
  | ExceptionClassSse5
  | ExceptionClassSse7
  | ExceptionClassAvx1
  | ExceptionClassAvx2
  | ExceptionClassAvx3
  | ExceptionClassAvx4
  | ExceptionClassAvx5
  | ExceptionClassAvx6
  | ExceptionClassAvx7
  | ExceptionClassAvx8
  | ExceptionClassAvx11
  | ExceptionClassAvx12
  | ExceptionClassE1
  | ExceptionClassE1Nf
  | ExceptionClassE2
  | ExceptionClassE2Nf
  | ExceptionClassE3
  | ExceptionClassE3Nf
  | ExceptionClassE4
  | ExceptionClassE4Nf
  | ExceptionClassE5
  | ExceptionClassE5Nf
  | ExceptionClassE6
  | ExceptionClassE6Nf
  | ExceptionClassE7Nm
  | ExceptionClassE7Nm128
  | ExceptionClassE9Nf
  | ExceptionClassE10
  | ExceptionClassE10Nf
  | ExceptionClassE11
  | ExceptionClassE11Nf
  | ExceptionClassE12
  | ExceptionClassE12Np
  | ExceptionClassK20
  | ExceptionClassK21
  deriving stock (Show, Eq, Bounded, Enum)
  deriving Storable via StorableExt ExceptionClass

