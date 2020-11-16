-- ISAExt.hs ---

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

module Zydis.ISAExt
  ( ISAExt(..)
  )
where

import           Zydis.Util

data ISAExt
  = ISAExtInvalid
  | ISAExtAdoxAdcx
  | ISAExtAes
  | ISAExtAmd3Dnow
  | ISAExtAvx
  | ISAExtAvx2
  | ISAExtAvx2Gather
  | ISAExtAvx512Evex
  | ISAExtAvx512Vex
  | ISAExtAvxaes
  | ISAExtBase
  | ISAExtBmi1
  | ISAExtBmi2
  | ISAExtCet
  | ISAExtCldemote
  | ISAExtClflushopt
  | ISAExtClfsh
  | ISAExtClwb
  | ISAExtClzero
  | ISAExtEnqcmd
  | ISAExtF16C
  | ISAExtFma
  | ISAExtFma4
  | ISAExtGfni
  | ISAExtInvpcid
  | ISAExtKnc
  | ISAExtKnce
  | ISAExtKncv
  | ISAExtLongmode
  | ISAExtLzcnt
  | ISAExtMmx
  | ISAExtMonitor
  | ISAExtMonitorx
  | ISAExtMovbe
  | ISAExtMovdir
  | ISAExtMpx
  | ISAExtPadlock
  | ISAExtPause
  | ISAExtPclmulqdq
  | ISAExtPconfig
  | ISAExtPku
  | ISAExtPrefetchwt1
  | ISAExtPt
  | ISAExtRdpid
  | ISAExtRdpru
  | ISAExtRdrand
  | ISAExtRdseed
  | ISAExtRdtscp
  | ISAExtRdwrfsgs
  | ISAExtRtm
  | ISAExtSgx
  | ISAExtSgxEnclv
  | ISAExtSha
  | ISAExtSmap
  | ISAExtSmx
  | ISAExtSse
  | ISAExtSse2
  | ISAExtSse3
  | ISAExtSse4
  | ISAExtSse4A
  | ISAExtSsse3
  | ISAExtSvm
  | ISAExtTbm
  | ISAExtVaes
  | ISAExtVmfunc
  | ISAExtVpclmulqdq
  | ISAExtVtx
  | ISAExtWaitpkg
  | ISAExtX87
  | ISAExtXop
  | ISAExtXsave
  | ISAExtXsavec
  | ISAExtXsaveopt
  | ISAExtXsaves
  deriving stock (Show, Eq, Bounded, Enum)
  deriving Storable via StorableExt ISAExt
