-- OperandEncoding.hs ---

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

module Zydis.OperandEncoding
  ( OperandEncoding(..)
  )
where

import           Zydis.Util

data OperandEncoding
  = OperandEncodingNone
  | OperandEncodingModrmReg
  | OperandEncodingModrmRm
  | OperandEncodingOpcode
  | OperandEncodingNdsndd
  | OperandEncodingIs4
  | OperandEncodingMask
  | OperandEncodingDisp8
  | OperandEncodingDisp16
  | OperandEncodingDisp32
  | OperandEncodingDisp64
  | OperandEncodingDisp163264
  | OperandEncodingDisp323264
  | OperandEncodingDisp163232
  | OperandEncodingUimm8
  | OperandEncodingUimm16
  | OperandEncodingUimm32
  | OperandEncodingUimm64
  | OperandEncodingUimm163264
  | OperandEncodingUimm323264
  | OperandEncodingUimm163232
  | OperandEncodingSimm8
  | OperandEncodingSimm16
  | OperandEncodingSimm32
  | OperandEncodingSimm64
  | OperandEncodingSimm163264
  | OperandEncodingSimm323264
  | OperandEncodingSimm163232
  | OperandEncodingJimm8
  | OperandEncodingJimm16
  | OperandEncodingJimm32
  | OperandEncodingJimm64
  | OperandEncodingJimm163264
  | OperandEncodingJimm323264
  | OperandEncodingJimm163232
  deriving stock (Show, Eq, Bounded, Enum)
  deriving Storable via StorableExt OperandEncoding
