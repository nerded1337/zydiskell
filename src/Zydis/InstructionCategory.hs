-- InstructionCategory.hs ---

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

module Zydis.InstructionCategory
  ( InstructionCategory(..)
  )
where

import           Zydis.Util

data InstructionCategory
  = InstructionCategoryInvalid
  | InstructionCategoryAdoxAdcx
  | InstructionCategoryAes
  | InstructionCategoryAmd3Dnow
  | InstructionCategoryAvx
  | InstructionCategoryAvx2
  | InstructionCategoryAvx2Gather
  | InstructionCategoryAvx512
  | InstructionCategoryAvx5124Fmaps
  | InstructionCategoryAvx5124Vnniw
  | InstructionCategoryAvx512Bitalg
  | InstructionCategoryAvx512Vbmi
  | InstructionCategoryAvx512Vp2Intersect
  | InstructionCategoryBinary
  | InstructionCategoryBitbyte
  | InstructionCategoryBlend
  | InstructionCategoryBmi1
  | InstructionCategoryBmi2
  | InstructionCategoryBroadcast
  | InstructionCategoryCall
  | InstructionCategoryCet
  | InstructionCategoryCldemote
  | InstructionCategoryClflushopt
  | InstructionCategoryClwb
  | InstructionCategoryClzero
  | InstructionCategoryCmov
  | InstructionCategoryCompress
  | InstructionCategoryCondBr
  | InstructionCategoryConflict
  | InstructionCategoryConvert
  | InstructionCategoryDataxfer
  | InstructionCategoryDecimal
  | InstructionCategoryEnqcmd
  | InstructionCategoryExpand
  | InstructionCategoryFcmov
  | InstructionCategoryFlagop
  | InstructionCategoryFma4
  | InstructionCategoryGather
  | InstructionCategoryGfni
  | InstructionCategoryIfma
  | InstructionCategoryInterrupt
  | InstructionCategoryIo
  | InstructionCategoryIostringop
  | InstructionCategoryKmask
  | InstructionCategoryKnc
  | InstructionCategoryKncmask
  | InstructionCategoryKncscalar
  | InstructionCategoryLogical
  | InstructionCategoryLogicalFp
  | InstructionCategoryLzcnt
  | InstructionCategoryMisc
  | InstructionCategoryMmx
  | InstructionCategoryMovdir
  | InstructionCategoryMpx
  | InstructionCategoryNop
  | InstructionCategoryPadlock
  | InstructionCategoryPclmulqdq
  | InstructionCategoryPconfig
  | InstructionCategoryPku
  | InstructionCategoryPop
  | InstructionCategoryPrefetch
  | InstructionCategoryPrefetchwt1
  | InstructionCategoryPt
  | InstructionCategoryPush
  | InstructionCategoryRdpid
  | InstructionCategoryRdpru
  | InstructionCategoryRdrand
  | InstructionCategoryRdseed
  | InstructionCategoryRdwrfsgs
  | InstructionCategoryRet
  | InstructionCategoryRotate
  | InstructionCategoryScatter
  | InstructionCategorySegop
  | InstructionCategorySemaphore
  | InstructionCategorySetcc
  | InstructionCategorySgx
  | InstructionCategorySha
  | InstructionCategoryShift
  | InstructionCategorySmap
  | InstructionCategorySse
  | InstructionCategoryStringop
  | InstructionCategorySttni
  | InstructionCategorySyscall
  | InstructionCategorySysret
  | InstructionCategorySystem
  | InstructionCategoryTbm
  | InstructionCategoryUfma
  | InstructionCategoryUncondBr
  | InstructionCategoryVaes
  | InstructionCategoryVbmi2
  | InstructionCategoryVfma
  | InstructionCategoryVpclmulqdq
  | InstructionCategoryVtx
  | InstructionCategoryWaitpkg
  | InstructionCategoryWidenop
  | InstructionCategoryX87Alu
  | InstructionCategoryXop
  | InstructionCategoryXsave
  | InstructionCategoryXsaveopt
  deriving stock (Show, Eq, Bounded, Enum)
  deriving Storable via StorableExt InstructionCategory
