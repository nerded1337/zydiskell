-- Operand.hs ---

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

{-# LANGUAGE DerivingStrategies #-}

module Zydis.Operand
  ( Operand(..)
  , OperandMemory(..)
  , OperandMemoryDisplacement(..)
  , OperandPointer(..)
  , OperandImmediate(..)
  )
where

import           Data.Int
import           Data.Word
import           Foreign.Storable
import qualified Foreign.Storable.Record       as Store

import           Zydis.ElementType
import           Zydis.OperandEncoding
import           Zydis.OperandMemoryType
import           Zydis.OperandType
import           Zydis.OperandVisibility
import           Zydis.Register


data OperandImmediate =
  OperandImmediate
  { operandImmediateIsSigned   :: {-# UNPACK #-}!Word8
  , operandImmediateIsRelative :: {-# UNPACK #-}!Word8
  , operandImmediateValue      :: {-# UNPACK #-}!Word64
  }
  deriving stock (Show, Eq)

decoderOperandImmediateStore :: Store.Dictionary OperandImmediate
decoderOperandImmediateStore =
  Store.run
    $   OperandImmediate
    <$> Store.element operandImmediateIsSigned
    <*> Store.element operandImmediateIsRelative
    <*> Store.element operandImmediateValue

instance Storable OperandImmediate where
  alignment = Store.alignment decoderOperandImmediateStore
  sizeOf    = Store.sizeOf decoderOperandImmediateStore
  peek      = Store.peek decoderOperandImmediateStore
  poke      = Store.poke decoderOperandImmediateStore

data OperandPointer =
  OperandPointer
  { operandPointerSegment :: {-# UNPACK #-}!Word16
  , operandPointerOffset  :: {-# UNPACK #-}!Word32
  }
  deriving stock (Show, Eq)

decoderOperandPointerStore :: Store.Dictionary OperandPointer
decoderOperandPointerStore =
  Store.run
    $   OperandPointer
    <$> Store.element operandPointerSegment
    <*> Store.element operandPointerOffset

instance Storable OperandPointer where
  alignment = Store.alignment decoderOperandPointerStore
  sizeOf    = Store.sizeOf decoderOperandPointerStore
  peek      = Store.peek decoderOperandPointerStore
  poke      = Store.poke decoderOperandPointerStore

data OperandMemoryDisplacement =
  OperandMemoryDisplacement
    { operandMemoryDisplacementHasDisplacement :: {-# UNPACK #-}!Word8
    , operandMemoryDisplacementValue           :: {-# UNPACK #-}!Int64
    }
  deriving stock (Show, Eq)

decoderOperandMemoryDisplacementStore
  :: Store.Dictionary OperandMemoryDisplacement
decoderOperandMemoryDisplacementStore =
  Store.run
    $   OperandMemoryDisplacement
    <$> Store.element operandMemoryDisplacementHasDisplacement
    <*> Store.element operandMemoryDisplacementValue

instance Storable OperandMemoryDisplacement where
  alignment = Store.alignment decoderOperandMemoryDisplacementStore
  sizeOf    = Store.sizeOf decoderOperandMemoryDisplacementStore
  peek      = Store.peek decoderOperandMemoryDisplacementStore
  poke      = Store.poke decoderOperandMemoryDisplacementStore

data OperandMemory =
  OperandMemory
    { operandMemoryType         :: !OperandMemoryType
    , operandMemorySegment      :: !Register
    , operandMemoryBase         :: !Register
    , operandMemoryIndex        :: !Register
    , operandMemoryScale        :: {-# UNPACK #-}!Word8
    , operandMemoryDisplacement :: {-# UNPACK #-}!OperandMemoryDisplacement
    }
  deriving stock (Show, Eq)

decoderOperandMemoryStore :: Store.Dictionary OperandMemory
decoderOperandMemoryStore =
  Store.run
    $   OperandMemory
    <$> Store.element operandMemoryType
    <*> Store.element operandMemorySegment
    <*> Store.element operandMemoryBase
    <*> Store.element operandMemoryIndex
    <*> Store.element operandMemoryScale
    <*> Store.element operandMemoryDisplacement

instance Storable OperandMemory where
  alignment = Store.alignment decoderOperandMemoryStore
  sizeOf    = Store.sizeOf decoderOperandMemoryStore
  peek      = Store.peek decoderOperandMemoryStore
  poke      = Store.poke decoderOperandMemoryStore

data Operand =
  Operand
    { operandId           :: {-# UNPACK #-}!Word8
    , operandType         :: !OperandType
    , operandVisibility   :: !OperandVisibility
    , operandActions      :: {-# UNPACK #-}!Word8
    , operandEncoding     :: !OperandEncoding
    , operandSize         :: {-# UNPACK #-}!Word16
    , operandElementType  :: !ElementType
    , operandElementSize  :: {-# UNPACK #-}!Word16
    , operandElementCount :: {-# UNPACK #-}!Word16
    , operandRegister     :: !Register
    , operandMemory       :: {-# UNPACK #-}!OperandMemory
    , operandPointer      :: {-# UNPACK #-}!OperandPointer
    , operandImmediate    :: {-# UNPACK #-}!OperandImmediate
    }
  deriving stock (Show, Eq)

decoderOperandStore :: Store.Dictionary Operand
decoderOperandStore =
  Store.run
    $   Operand
    <$> Store.element operandId
    <*> Store.element operandType
    <*> Store.element operandVisibility
    <*> Store.element operandActions
    <*> Store.element operandEncoding
    <*> Store.element operandSize
    <*> Store.element operandElementType
    <*> Store.element operandElementSize
    <*> Store.element operandElementCount
    <*> Store.element operandRegister
    <*> Store.element operandMemory
    <*> Store.element operandPointer
    <*> Store.element operandImmediate

instance Storable Operand where
  alignment = Store.alignment decoderOperandStore
  sizeOf    = Store.sizeOf decoderOperandStore
  peek      = Store.peek decoderOperandStore
  poke      = Store.poke decoderOperandStore
