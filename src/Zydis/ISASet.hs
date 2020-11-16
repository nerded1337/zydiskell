-- ISASet.hs ---

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

module Zydis.ISASet
  ( ISASet(..)
  )
where

import           Zydis.Util

data ISASet
  = ISASetInvalid
  | ISASetAdoxAdcx
  | ISASetAes
  | ISASetAmd
  | ISASetAmd3Dnow
  | ISASetAvx
  | ISASetAvx2
  | ISASetAvx2Gather
  | ISASetAvx512Bw128
  | ISASetAvx512Bw128N
  | ISASetAvx512Bw256
  | ISASetAvx512Bw512
  | ISASetAvx512BwKop
  | ISASetAvx512Cd128
  | ISASetAvx512Cd256
  | ISASetAvx512Cd512
  | ISASetAvx512Dq128
  | ISASetAvx512Dq128N
  | ISASetAvx512Dq256
  | ISASetAvx512Dq512
  | ISASetAvx512DqKop
  | ISASetAvx512DqScalar
  | ISASetAvx512Er512
  | ISASetAvx512ErScalar
  | ISASetAvx512F128
  | ISASetAvx512F128N
  | ISASetAvx512F256
  | ISASetAvx512F512
  | ISASetAvx512FKop
  | ISASetAvx512FScalar
  | ISASetAvx512Pf512
  | ISASetAvx5124Fmaps512
  | ISASetAvx5124FmapsScalar
  | ISASetAvx5124Vnniw512
  | ISASetAvx512Bf16128
  | ISASetAvx512Bf16256
  | ISASetAvx512Bf16512
  | ISASetAvx512Bitalg128
  | ISASetAvx512Bitalg256
  | ISASetAvx512Bitalg512
  | ISASetAvx512Gfni128
  | ISASetAvx512Gfni256
  | ISASetAvx512Gfni512
  | ISASetAvx512Ifma128
  | ISASetAvx512Ifma256
  | ISASetAvx512Ifma512
  | ISASetAvx512Vaes128
  | ISASetAvx512Vaes256
  | ISASetAvx512Vaes512
  | ISASetAvx512Vbmi2128
  | ISASetAvx512Vbmi2256
  | ISASetAvx512Vbmi2512
  | ISASetAvx512Vbmi128
  | ISASetAvx512Vbmi256
  | ISASetAvx512Vbmi512
  | ISASetAvx512Vnni128
  | ISASetAvx512Vnni256
  | ISASetAvx512Vnni512
  | ISASetAvx512Vp2Intersect128
  | ISASetAvx512Vp2Intersect256
  | ISASetAvx512Vp2Intersect512
  | ISASetAvx512Vpclmulqdq128
  | ISASetAvx512Vpclmulqdq256
  | ISASetAvx512Vpclmulqdq512
  | ISASetAvx512Vpopcntdq128
  | ISASetAvx512Vpopcntdq256
  | ISASetAvx512Vpopcntdq512
  | ISASetAvxaes
  | ISASetAvxGfni
  | ISASetBmi1
  | ISASetBmi2
  | ISASetCet
  | ISASetCldemote
  | ISASetClflushopt
  | ISASetClfsh
  | ISASetClwb
  | ISASetClzero
  | ISASetCmov
  | ISASetCmpxchg16B
  | ISASetEnqcmd
  | ISASetF16C
  | ISASetFatNop
  | ISASetFcmov
  | ISASetFma
  | ISASetFma4
  | ISASetFxsave
  | ISASetFxsave64
  | ISASetGfni
  | ISASetI186
  | ISASetI286Protected
  | ISASetI286Real
  | ISASetI386
  | ISASetI486
  | ISASetI486Real
  | ISASetI86
  | ISASetInvpcid
  | ISASetKnce
  | ISASetKncjkbr
  | ISASetKncstream
  | ISASetKncv
  | ISASetKncMisc
  | ISASetKncPfHint
  | ISASetLahf
  | ISASetLongmode
  | ISASetLzcnt
  | ISASetMonitor
  | ISASetMonitorx
  | ISASetMovbe
  | ISASetMovdir
  | ISASetMpx
  | ISASetPadlockAce
  | ISASetPadlockPhe
  | ISASetPadlockPmm
  | ISASetPadlockRng
  | ISASetPause
  | ISASetPclmulqdq
  | ISASetPconfig
  | ISASetPentiummmx
  | ISASetPentiumreal
  | ISASetPku
  | ISASetPopcnt
  | ISASetPpro
  | ISASetPrefetchwt1
  | ISASetPrefetchNop
  | ISASetPt
  | ISASetRdpid
  | ISASetRdpmc
  | ISASetRdpru
  | ISASetRdrand
  | ISASetRdseed
  | ISASetRdtscp
  | ISASetRdwrfsgs
  | ISASetRtm
  | ISASetSgx
  | ISASetSgxEnclv
  | ISASetSha
  | ISASetSmap
  | ISASetSmx
  | ISASetSse
  | ISASetSse2
  | ISASetSse2Mmx
  | ISASetSse3
  | ISASetSse3X87
  | ISASetSse4
  | ISASetSse42
  | ISASetSse4A
  | ISASetSsemxcsr
  | ISASetSsePrefetch
  | ISASetSsse3
  | ISASetSsse3Mmx
  | ISASetSvm
  | ISASetTbm
  | ISASetVaes
  | ISASetVmfunc
  | ISASetVpclmulqdq
  | ISASetVtx
  | ISASetWaitpkg
  | ISASetX87
  | ISASetXop
  | ISASetXsave
  | ISASetXsavec
  | ISASetXsaveopt
  | ISASetXsaves
  deriving stock (Show, Eq, Bounded, Enum)
  deriving Storable via StorableExt ISASet
