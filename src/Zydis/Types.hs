-- Types.hs ---

-- Copyright (C) 2020 Nerd Ed

-- Author: Nerd Ed <nerded.nerded @gmail.com>

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

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Zydis.Types
  ( MachineMode(..)
  , AddressWidth(..)
  , Decoder(..)
  , DecodedInstruction(..)
  , module Z
  )
where

import           Data.Int
import           Data.Word
import           Foreign.Storable
import qualified Foreign.Storable.Record       as Store
import           GHC.TypeLits

import           Zydis.AddressWidth            as Z
import           Zydis.BranchType              as Z
import           Zydis.BroadcastMode           as Z
import           Zydis.Constants               as Z
import           Zydis.ConversionMode          as Z
import           Zydis.CPUFlagAction           as Z
import           Zydis.ExceptionClass          as Z
import           Zydis.InstructionCategory     as Z
import           Zydis.InstructionEncoding     as Z
import           Zydis.ISAExt                  as Z
import           Zydis.ISASet                  as Z
import           Zydis.MachineMode             as Z
import           Zydis.MaskMode                as Z
import           Zydis.Mnemonic                as Z
import           Zydis.OpcodeMap               as Z
import           Zydis.Operand                 as Z
import           Zydis.PrefixType              as Z
import           Zydis.Register                as Z
import           Zydis.RoundingMode            as Z
import           Zydis.SwizzleMode             as Z
import           Zydis.Util                    as Z

data DecodedInstructionRawImmediate =
  DecodedInstructionRawImmediate
  { decodedInstructionRawImmediateIsSigned   :: !Word8
  , decodedInstructionRawImmediateIsRelative :: !Word8
  , decodedInstructionRawImmediateValue      :: {-# UNPACK #-}!Word64
  , decodedInstructionRawImmediateSize       :: {-# UNPACK #-}!Word8
  , decodedInstructionRawImmediateOffset     :: {-# UNPACK #-}!Word8
  }
  deriving stock (Show, Eq)

decoderInstructionRawImmediateStore
  :: Store.Dictionary DecodedInstructionRawImmediate
decoderInstructionRawImmediateStore =
  Store.run
    $   DecodedInstructionRawImmediate
    <$> Store.element decodedInstructionRawImmediateIsSigned
    <*> Store.element decodedInstructionRawImmediateIsRelative
    <*> Store.element decodedInstructionRawImmediateValue
    <*> Store.element decodedInstructionRawImmediateSize
    <*> Store.element decodedInstructionRawImmediateOffset

instance Storable DecodedInstructionRawImmediate where
  alignment = Store.alignment decoderInstructionRawImmediateStore
  sizeOf    = Store.sizeOf decoderInstructionRawImmediateStore
  peek      = Store.peek decoderInstructionRawImmediateStore
  poke      = Store.poke decoderInstructionRawImmediateStore

data DecodedInstructionRawDisp =
  DecodedInstructionRawDisp
  { decodedInstructionRawDispValue  :: {-# UNPACK #-}!Int64
  , decodedInstructionRawDispSize   :: {-# UNPACK #-}!Word8
  , decodedInstructionRawDispOffset :: {-# UNPACK #-}!Word8
  }
  deriving stock (Show, Eq)

decoderInstructionRawDispStore :: Store.Dictionary DecodedInstructionRawDisp
decoderInstructionRawDispStore =
  Store.run
    $   DecodedInstructionRawDisp
    <$> Store.element decodedInstructionRawDispValue
    <*> Store.element decodedInstructionRawDispSize
    <*> Store.element decodedInstructionRawDispOffset

instance Storable DecodedInstructionRawDisp where
  alignment = Store.alignment decoderInstructionRawDispStore
  sizeOf    = Store.sizeOf decoderInstructionRawDispStore
  peek      = Store.peek decoderInstructionRawDispStore
  poke      = Store.poke decoderInstructionRawDispStore

data DecodedInstructionRawSib =
  DecodedInstructionRawSib
  { decodedInstructionRawSibScale  :: {-# UNPACK #-}!Word8
  , decodedInstructionRawSibIndex  :: {-# UNPACK #-}!Word8
  , decodedInstructionRawSibBase   :: {-# UNPACK #-}!Word8
  , decodedInstructionRawSibOffset :: {-# UNPACK #-}!Word8
  }
  deriving stock (Show, Eq)

decodedInstructionRawSibStore :: Store.Dictionary DecodedInstructionRawSib
decodedInstructionRawSibStore =
  Store.run
    $   DecodedInstructionRawSib
    <$> Store.element decodedInstructionRawSibScale
    <*> Store.element decodedInstructionRawSibIndex
    <*> Store.element decodedInstructionRawSibBase
    <*> Store.element decodedInstructionRawSibOffset

instance Storable DecodedInstructionRawSib where
  alignment = Store.alignment decodedInstructionRawSibStore
  sizeOf    = Store.sizeOf decodedInstructionRawSibStore
  peek      = Store.peek decodedInstructionRawSibStore
  poke      = Store.poke decodedInstructionRawSibStore

data DecodedInstructionModRm =
  DecodedInstructionModRm
  { decodedInstructionModRmAddressingMode :: {-# UNPACK #-}!Word8
  , decodedInstructionModRmRegister       :: {-# UNPACK #-}!Word8
  , decodedInstructionModRmRM             :: {-# UNPACK #-}!Word8
  , decodedInstructionModRmOffset         :: {-# UNPACK #-}!Word8
  }
  deriving stock (Show, Eq)

decodedInstructionModRmStore :: Store.Dictionary DecodedInstructionModRm
decodedInstructionModRmStore =
  Store.run
    $   DecodedInstructionModRm
    <$> Store.element decodedInstructionModRmAddressingMode
    <*> Store.element decodedInstructionModRmRegister
    <*> Store.element decodedInstructionModRmRM
    <*> Store.element decodedInstructionModRmOffset

instance Storable DecodedInstructionModRm where
  alignment = Store.alignment decodedInstructionModRmStore
  sizeOf    = Store.sizeOf decodedInstructionModRmStore
  peek      = Store.peek decodedInstructionModRmStore
  poke      = Store.poke decodedInstructionModRmStore

data DecodedInstructionRawMvex =
  DecodedInstructionRawMvex
  { decodedInstructionRawMvexR      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawMvexX      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawMvexB      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawMvexR2     :: {-# UNPACK #-}!Word8
  , decodedInstructionRawMvexMMMM   :: {-# UNPACK #-}!Word8
  , decodedInstructionRawMvexW      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawMvexVVVV   :: {-# UNPACK #-}!Word8
  , decodedInstructionRawMvexPP     :: {-# UNPACK #-}!Word8
  , decodedInstructionRawMvexE      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawMvexSSS    :: {-# UNPACK #-}!Word8
  , decodedInstructionRawMvexV2     :: {-# UNPACK #-}!Word8
  , decodedInstructionRawMvexKKK    :: {-# UNPACK #-}!Word8
  , decodedInstructionRawMvexOffset :: {-# UNPACK #-}!Word8
  }
  deriving stock (Show, Eq)

decodedInstructionRawMvexStore :: Store.Dictionary DecodedInstructionRawMvex
decodedInstructionRawMvexStore =
  Store.run
    $   DecodedInstructionRawMvex
    <$> Store.element decodedInstructionRawMvexR
    <*> Store.element decodedInstructionRawMvexX
    <*> Store.element decodedInstructionRawMvexB
    <*> Store.element decodedInstructionRawMvexR2
    <*> Store.element decodedInstructionRawMvexMMMM
    <*> Store.element decodedInstructionRawMvexW
    <*> Store.element decodedInstructionRawMvexVVVV
    <*> Store.element decodedInstructionRawMvexPP
    <*> Store.element decodedInstructionRawMvexE
    <*> Store.element decodedInstructionRawMvexSSS
    <*> Store.element decodedInstructionRawMvexV2
    <*> Store.element decodedInstructionRawMvexKKK
    <*> Store.element decodedInstructionRawMvexOffset

instance Storable DecodedInstructionRawMvex where
  alignment = Store.alignment decodedInstructionRawMvexStore
  sizeOf    = Store.sizeOf decodedInstructionRawMvexStore
  peek      = Store.peek decodedInstructionRawMvexStore
  poke      = Store.poke decodedInstructionRawMvexStore

data DecodedInstructionRawEvex =
  DecodedInstructionRawEvex
  { decodedInstructionRawEvexR      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexX      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexB      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexR2     :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexMM     :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexW      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexVVVV   :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexPP     :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexZ      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexL2     :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexL      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexB'     :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexV2     :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexAAA    :: {-# UNPACK #-}!Word8
  , decodedInstructionRawEvexOffset :: {-# UNPACK #-}!Word8
  }
  deriving stock (Show, Eq)

decodedInstructionRawEvexStore :: Store.Dictionary DecodedInstructionRawEvex
decodedInstructionRawEvexStore =
  Store.run
    $   DecodedInstructionRawEvex
    <$> Store.element decodedInstructionRawEvexR
    <*> Store.element decodedInstructionRawEvexX
    <*> Store.element decodedInstructionRawEvexB
    <*> Store.element decodedInstructionRawEvexR2
    <*> Store.element decodedInstructionRawEvexMM
    <*> Store.element decodedInstructionRawEvexW
    <*> Store.element decodedInstructionRawEvexVVVV
    <*> Store.element decodedInstructionRawEvexPP
    <*> Store.element decodedInstructionRawEvexZ
    <*> Store.element decodedInstructionRawEvexL2
    <*> Store.element decodedInstructionRawEvexL
    <*> Store.element decodedInstructionRawEvexB'
    <*> Store.element decodedInstructionRawEvexV2
    <*> Store.element decodedInstructionRawEvexAAA
    <*> Store.element decodedInstructionRawEvexOffset

instance Storable DecodedInstructionRawEvex where
  alignment = Store.alignment decodedInstructionRawEvexStore
  sizeOf    = Store.sizeOf decodedInstructionRawEvexStore
  peek      = Store.peek decodedInstructionRawEvexStore
  poke      = Store.poke decodedInstructionRawEvexStore

data DecodedInstructionRawVex =
  DecodedInstructionRawVex
  { decodedInstructionRawVexR      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawVexX      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawVexB      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawVexMMMMM  :: {-# UNPACK #-}!Word8
  , decodedInstructionRawVexW      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawVexVVVV   :: {-# UNPACK #-}!Word8
  , decodedInstructionRawVexL      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawVexPP     :: {-# UNPACK #-}!Word8
  , decodedInstructionRawVexOffset :: {-# UNPACK #-}!Word8
  , decodedInstructionRawVexSize   :: {-# UNPACK #-}!Word8
  }
  deriving stock (Show, Eq)

decodedInstructionRawVexStore :: Store.Dictionary DecodedInstructionRawVex
decodedInstructionRawVexStore =
  Store.run
    $   DecodedInstructionRawVex
    <$> Store.element decodedInstructionRawVexR
    <*> Store.element decodedInstructionRawVexX
    <*> Store.element decodedInstructionRawVexB
    <*> Store.element decodedInstructionRawVexMMMMM
    <*> Store.element decodedInstructionRawVexW
    <*> Store.element decodedInstructionRawVexVVVV
    <*> Store.element decodedInstructionRawVexL
    <*> Store.element decodedInstructionRawVexPP
    <*> Store.element decodedInstructionRawVexOffset
    <*> Store.element decodedInstructionRawVexSize

instance Storable DecodedInstructionRawVex where
  alignment = Store.alignment decodedInstructionRawVexStore
  sizeOf    = Store.sizeOf decodedInstructionRawVexStore
  peek      = Store.peek decodedInstructionRawVexStore
  poke      = Store.poke decodedInstructionRawVexStore

data DecodedInstructionRawXop =
  DecodedInstructionRawXop
  { decodedInstructionRawXopR      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawXopX      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawXopB      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawXopMMMMM  :: {-# UNPACK #-}!Word8
  , decodedInstructionRawXopW      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawXopVVVV   :: {-# UNPACK #-}!Word8
  , decodedInstructionRawXopL      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawXopPP     :: {-# UNPACK #-}!Word8
  , decodedInstructionRawXopOffset :: {-# UNPACK #-}!Word8
  }
  deriving stock (Show, Eq)


decodedInstructionRawXopStore :: Store.Dictionary DecodedInstructionRawXop
decodedInstructionRawXopStore =
  Store.run
    $   DecodedInstructionRawXop
    <$> Store.element decodedInstructionRawXopR
    <*> Store.element decodedInstructionRawXopX
    <*> Store.element decodedInstructionRawXopB
    <*> Store.element decodedInstructionRawXopMMMMM
    <*> Store.element decodedInstructionRawXopW
    <*> Store.element decodedInstructionRawXopVVVV
    <*> Store.element decodedInstructionRawXopL
    <*> Store.element decodedInstructionRawXopPP
    <*> Store.element decodedInstructionRawXopOffset

instance Storable DecodedInstructionRawXop where
  alignment = Store.alignment decodedInstructionRawXopStore
  sizeOf    = Store.sizeOf decodedInstructionRawXopStore
  peek      = Store.peek decodedInstructionRawXopStore
  poke      = Store.poke decodedInstructionRawXopStore

data DecodedInstructionRawRex =
  DecodedInstructionRawRex
  { decodedInstructionRawRexW      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawRexR      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawRexX      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawRexB      :: {-# UNPACK #-}!Word8
  , decodedInstructionRawRexOffset :: {-# UNPACK #-}!Word8
  }
  deriving stock (Show, Eq)

decodedInstructionRawRexStore :: Store.Dictionary DecodedInstructionRawRex
decodedInstructionRawRexStore =
  Store.run
    $   DecodedInstructionRawRex
    <$> Store.element decodedInstructionRawRexW
    <*> Store.element decodedInstructionRawRexR
    <*> Store.element decodedInstructionRawRexX
    <*> Store.element decodedInstructionRawRexB
    <*> Store.element decodedInstructionRawRexOffset

instance Storable DecodedInstructionRawRex where
  alignment = Store.alignment decodedInstructionRawRexStore
  sizeOf    = Store.sizeOf decodedInstructionRawRexStore
  peek      = Store.peek decodedInstructionRawRexStore
  poke      = Store.poke decodedInstructionRawRexStore

data DecodedInstructionRawPrefix =
  DecodedInstructionRawPrefix
  { decodedInstructionRawPrefixType  :: !PrefixType
  , decodedInstructionRawPrefixValue :: {-# UNPACK #-}!Word8
  }
  deriving stock (Show, Eq)

decodedInstructionRawPrefixStore :: Store.Dictionary DecodedInstructionRawPrefix
decodedInstructionRawPrefixStore =
  Store.run
    $   DecodedInstructionRawPrefix
    <$> Store.element decodedInstructionRawPrefixType
    <*> Store.element decodedInstructionRawPrefixValue

instance Storable DecodedInstructionRawPrefix where
  alignment = Store.alignment decodedInstructionRawPrefixStore
  sizeOf    = Store.sizeOf decodedInstructionRawPrefixStore
  peek      = Store.peek decodedInstructionRawPrefixStore
  poke      = Store.poke decodedInstructionRawPrefixStore

data DecodedInstructionRaw =
  DecodedInstructionRaw
  { decodedInstructionRawPrefixCount :: {-# UNPACK #-}!Word8
  , decodedInstructionRawPrefixes    :: !(StorableFixedArray DecodedInstructionRawPrefix ZydisMaxInstructionLength)
  , decodedInstructionRawRex         :: !DecodedInstructionRawRex
  , decodedInstructionRawXop         :: !DecodedInstructionRawXop
  , decodedInstructionRawVex         :: !DecodedInstructionRawVex
  , decodedInstructionRawEvex        :: !DecodedInstructionRawEvex
  , decodedInstructionRawMvex        :: !DecodedInstructionRawMvex
  , decodedInstructionRawModRm       :: !DecodedInstructionModRm
  , decodedInstructionRawSib         :: !DecodedInstructionRawSib
  , decodedInstructionRawDisp        :: !DecodedInstructionRawDisp
  , decodedInstructionRawImmediates  :: !(StorableFixedArray DecodedInstructionRawImmediate ZydisRawImmediateCount)
  }
  deriving stock (Show, Eq)

decodedInstructionRawStore :: Store.Dictionary DecodedInstructionRaw
decodedInstructionRawStore =
  Store.run
    $   DecodedInstructionRaw
    <$> Store.element decodedInstructionRawPrefixCount
    <*> Store.element decodedInstructionRawPrefixes
    <*> Store.element decodedInstructionRawRex
    <*> Store.element decodedInstructionRawXop
    <*> Store.element decodedInstructionRawVex
    <*> Store.element decodedInstructionRawEvex
    <*> Store.element decodedInstructionRawMvex
    <*> Store.element decodedInstructionRawModRm
    <*> Store.element decodedInstructionRawSib
    <*> Store.element decodedInstructionRawDisp
    <*> Store.element decodedInstructionRawImmediates

instance Storable DecodedInstructionRaw where
  alignment = Store.alignment decodedInstructionRawStore
  sizeOf    = Store.sizeOf decodedInstructionRawStore
  peek      = Store.peek decodedInstructionRawStore
  poke      = Store.poke decodedInstructionRawStore

data DecodedInstructionMeta =
  DecodedInstructionMeta
    { decodedInstructionMetaCategory   :: !InstructionCategory
    , decodedInstructionMetaISASet     :: !ISASet
    , decodedInstructionMetaISAExt     :: !ISAExt
    , decodedInstructionBranchType     :: !BranchType
    , decodedInstructionExceptionClass :: !ExceptionClass
    }
  deriving stock (Show, Eq)

decodedInstructionMetaStore :: Store.Dictionary DecodedInstructionMeta
decodedInstructionMetaStore =
  Store.run
    $   DecodedInstructionMeta
    <$> Store.element decodedInstructionMetaCategory
    <*> Store.element decodedInstructionMetaISASet
    <*> Store.element decodedInstructionMetaISAExt
    <*> Store.element decodedInstructionBranchType
    <*> Store.element decodedInstructionExceptionClass

instance Storable DecodedInstructionMeta where
  alignment = Store.alignment decodedInstructionMetaStore
  sizeOf    = Store.sizeOf decodedInstructionMetaStore
  peek      = Store.peek decodedInstructionMetaStore
  poke      = Store.poke decodedInstructionMetaStore

data DecodedInstructionAvxBroadcast =
  DecodedInstructionAvxBroadcast
    { decodedInstructionAvxBroadcastIsStatic :: !Word8
    , decodedInstructionAvxBroadcastMode     :: !BroadcastMode
    }
  deriving stock (Show, Eq)

decodedInstructionAvxBroadcastStore
  :: Store.Dictionary DecodedInstructionAvxBroadcast
decodedInstructionAvxBroadcastStore =
  Store.run
    $   DecodedInstructionAvxBroadcast
    <$> Store.element decodedInstructionAvxBroadcastIsStatic
    <*> Store.element decodedInstructionAvxBroadcastMode

instance Storable DecodedInstructionAvxBroadcast where
  alignment = Store.alignment decodedInstructionAvxBroadcastStore
  sizeOf    = Store.sizeOf decodedInstructionAvxBroadcastStore
  peek      = Store.peek decodedInstructionAvxBroadcastStore
  poke      = Store.poke decodedInstructionAvxBroadcastStore

data DecodedInstructionAvxMask =
  DecodedInstructionAvxMask
    { decodedInstructionAvxMaskMode :: !MaskMode
    , decodedInstructionAvxRegister :: !Register
    }
  deriving stock (Show, Eq)

decodedInstructionAvxMaskStore :: Store.Dictionary DecodedInstructionAvxMask
decodedInstructionAvxMaskStore =
  Store.run
    $   DecodedInstructionAvxMask
    <$> Store.element decodedInstructionAvxMaskMode
    <*> Store.element decodedInstructionAvxRegister

instance Storable DecodedInstructionAvxMask where
  alignment = Store.alignment decodedInstructionAvxMaskStore
  sizeOf    = Store.sizeOf decodedInstructionAvxMaskStore
  peek      = Store.peek decodedInstructionAvxMaskStore
  poke      = Store.poke decodedInstructionAvxMaskStore

data DecodedInstructionAvx =
  DecodedInstructionAvx
    { decodedInstructionAvxVectorLength    :: {-# UNPACK #-}!Word16
    , decodedInstructionAvxMask            :: !DecodedInstructionAvxMask
    , decodedInstructionAvxBroadcast       :: !DecodedInstructionAvxBroadcast
    , decodedInstructionAvxRoundingMode    :: !RoundingMode
    , decodedInstructionAvxSwizzleMode     :: !SwizzleMode
    , decodedInstructionAvxConversionMode  :: !ConversionMode
    , decodedInstructionAvxHasSAE          :: !Word8
    , decodedInstructionAvxHasEvictionHint :: !Word8
    }
  deriving stock (Show, Eq)

decodedInstructionAvxStore :: Store.Dictionary DecodedInstructionAvx
decodedInstructionAvxStore =
  Store.run
    $   DecodedInstructionAvx
    <$> Store.element decodedInstructionAvxVectorLength
    <*> Store.element decodedInstructionAvxMask
    <*> Store.element decodedInstructionAvxBroadcast
    <*> Store.element decodedInstructionAvxRoundingMode
    <*> Store.element decodedInstructionAvxSwizzleMode
    <*> Store.element decodedInstructionAvxConversionMode
    <*> Store.element decodedInstructionAvxHasSAE
    <*> Store.element decodedInstructionAvxHasEvictionHint

instance Storable DecodedInstructionAvx where
  alignment = Store.alignment decodedInstructionAvxStore
  sizeOf    = Store.sizeOf decodedInstructionAvxStore
  peek      = Store.peek decodedInstructionAvxStore
  poke      = Store.poke decodedInstructionAvxStore

data DecodedInstruction =
  DecodedInstruction
    { decodedInstructionMachineMode   :: !MachineMode
    , decodedInstructionMnemonic      :: !Mnemonic
    , decodedInstructionLength        :: {-# UNPACK #-}!Word8
    , decodedInstructionEncoding      :: !InstructionEncoding
    , decodedInstructionOpcodeMap     :: !OpcodeMap
    , decodedInstructionOpcode        :: {-# UNPACK #-}!Word8
    , decodedInstructionStackWidth    :: {-# UNPACK #-}!Word8
    , decodedInstructionOperandWidth  :: {-# UNPACK #-}!Word8
    , decodedInstructionAddressWidth  :: {-# UNPACK #-}!Word8
    , decodedInstructionOperandCount  :: {-# UNPACK #-}!Word8
    , decodedInstructionOperands      :: !(StorableFixedArray Operand ZydisMaxOperandCount)
    , decodedInstructionAttributes    :: {-# UNPACK #-}!Word64
    , decodedInstructionAccessedFlags :: !(StorableFixedArray CPUFlagAction (ZydisCpuFlagMaxValue + 1))
    , decodedInstructionAvx           :: !DecodedInstructionAvx
    , decodedInstructionMeta          :: !DecodedInstructionMeta
    , decodedInstructionRaw           :: !DecodedInstructionRaw
    }
  deriving stock (Show, Eq)

decodedInstructionStore :: Store.Dictionary DecodedInstruction
decodedInstructionStore =
  Store.run
    $   DecodedInstruction
    <$> Store.element decodedInstructionMachineMode
    <*> Store.element decodedInstructionMnemonic
    <*> Store.element decodedInstructionLength
    <*> Store.element decodedInstructionEncoding
    <*> Store.element decodedInstructionOpcodeMap
    <*> Store.element decodedInstructionOpcode
    <*> Store.element decodedInstructionStackWidth
    <*> Store.element decodedInstructionOperandWidth
    <*> Store.element decodedInstructionAddressWidth
    <*> Store.element decodedInstructionOperandCount
    <*> Store.element decodedInstructionOperands
    <*> Store.element decodedInstructionAttributes
    <*> Store.element decodedInstructionAccessedFlags
    <*> Store.element decodedInstructionAvx
    <*> Store.element decodedInstructionMeta
    <*> Store.element decodedInstructionRaw

instance Storable DecodedInstruction where
  alignment = Store.alignment decodedInstructionStore
  sizeOf    = Store.sizeOf decodedInstructionStore
  peek      = Store.peek decodedInstructionStore
  poke      = Store.poke decodedInstructionStore

data Decoder =
  Decoder
    { decoderMachineMode  :: !MachineMode
    , decoderAddressWidth :: !AddressWidth
    , decoderDecoderMode  :: !Word8
    }
  deriving stock (Show, Eq)

decoderStore :: Store.Dictionary Decoder
decoderStore =
  Store.run
    $   Decoder
    <$> Store.element decoderMachineMode
    <*> Store.element decoderAddressWidth
    <*> Store.element decoderDecoderMode

instance Storable Decoder where
  alignment = Store.alignment decoderStore
  sizeOf    = Store.sizeOf decoderStore
  peek      = Store.peek decoderStore
  poke      = Store.poke decoderStore
