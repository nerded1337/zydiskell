-- Status.hs ---

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

-- | This module implement the zycore/zydis status codes.
--
-- Zycore ref: https://github.com/zyantific/zycore-c/blob/71440fa634d1313db735d3262d453be641bb404f/include/Zycore/Status.h#L134
--
-- Zydis ref: https://github.com/zyantific/zydis/blob/675c90ee7bc80d1aa6273e5ecfba0bd293bf89e9/include/Zydis/Status.h#L55
--
module Zydis.Status
  ( ZyanCoreStatus(..)
  , ZydisStatus(..)
  , ZyanStatus
  , ZyanNativeStatus
  , fromZyanNativeStatus
  )
where

import           Data.Bits
import           Data.Word

data ZyanCoreStatus
  = ZyanCoreStatusSuccess
  | ZyanCoreStatusFailed
  | ZyanCoreStatusTrue
  | ZyanCoreStatusFalse
  | ZyanCoreStatusInvalidArgument
  | ZyanCoreStatusInvalidOperation
  | ZyanCoreStatusAccessDenied
  | ZyanCoreStatusNotFound
  | ZyanCoreStatusOutOfRange
  | ZyanCoreStatusInsufficientBufferSize
  | ZyanCoreStatusNotEnoughMemory
  | ZyanCoreStatusBadSystemCall
  | ZyanCoreStatusOutOfResources
  | ZyanCoreStatusMissingDependency
  deriving stock (Show, Eq)

data ZydisStatus
  = ZydisStatusInvalidMask
  | ZydisStatusMalformedMvex
  | ZydisStatusMalformedEvex
  | ZydisStatusInvalidMap
  | ZydisStatusIllegalRex
  | ZydisStatusIllegalLegacyPFX
  | ZydisStatusIllegalLock
  | ZydisStatusBadRegister
  | ZydisStatusInstructionTooLong
  | ZydisStatusNoMoreData
  | ZydisStatusDecodingError
  | ZydisStatusSkipToken
  deriving stock (Show, Eq)

type ZyanStatus = Either ZyanCoreStatus ZydisStatus

type IsError = Bool

type ZyanNativeModule = Word16

type ZyanNativeStatusCode = Word32

type ZyanNativeStatus = Word32

-- | Combine the values into a unique error code.
--
-- This function is a direct copy of https://github.com/zyantific/zycore-c/blob/71440fa634d1313db735d3262d453be641bb404f/include/Zycore/Status.h#L67
--
makeZyanNativeStatus
  :: IsError -> ZyanNativeModule -> ZyanNativeStatusCode -> ZyanNativeStatus
makeZyanNativeStatus e m c =
  ((if e then 1 else 0) `shiftL` 31)
    .|. ((fromIntegral m .&. 0x7FF) `shiftL` 20)
    .|. (c .&. 0xFFFFF)
{-# INLINE makeZyanNativeStatus #-}

-- * Core status

zyanModuleCore :: ZyanNativeModule
zyanModuleCore = 0x001
{-# INLINE zyanModuleCore #-}

zyanCoreStatusSuccess :: ZyanNativeStatus
zyanCoreStatusSuccess = makeZyanNativeStatus False zyanModuleCore 0x00
{-# INLINE zyanCoreStatusSuccess #-}

zyanCoreStatusFailed :: ZyanNativeStatus
zyanCoreStatusFailed = makeZyanNativeStatus True zyanModuleCore 0x01
{-# INLINE zyanCoreStatusFailed #-}

zyanCoreStatusTrue :: ZyanNativeStatus
zyanCoreStatusTrue = makeZyanNativeStatus False zyanModuleCore 0x02
{-# INLINE zyanCoreStatusTrue #-}

zyanCoreStatusFalse :: ZyanNativeStatus
zyanCoreStatusFalse = makeZyanNativeStatus False zyanModuleCore 0x03
{-# INLINE zyanCoreStatusFalse #-}

zyanCoreStatusInvalidArgument :: ZyanNativeStatus
zyanCoreStatusInvalidArgument = makeZyanNativeStatus True zyanModuleCore 0x04
{-# INLINE zyanCoreStatusInvalidArgument #-}

zyanCoreStatusInvalidOperation :: ZyanNativeStatus
zyanCoreStatusInvalidOperation = makeZyanNativeStatus True zyanModuleCore 0x05
{-# INLINE zyanCoreStatusInvalidOperation #-}

zyanCoreStatusAccessDenied :: ZyanNativeStatus
zyanCoreStatusAccessDenied = makeZyanNativeStatus True zyanModuleCore 0x06
{-# INLINE zyanCoreStatusAccessDenied #-}

zyanCoreStatusNotFound :: ZyanNativeStatus
zyanCoreStatusNotFound = makeZyanNativeStatus True zyanModuleCore 0x07
{-# INLINE zyanCoreStatusNotFound #-}

zyanCoreStatusOutOfRange :: ZyanNativeStatus
zyanCoreStatusOutOfRange = makeZyanNativeStatus True zyanModuleCore 0x08
{-# INLINE zyanCoreStatusOutOfRange #-}

zyanCoreStatusInsufficientBufferSize :: ZyanNativeStatus
zyanCoreStatusInsufficientBufferSize =
  makeZyanNativeStatus True zyanModuleCore 0x09
{-# INLINE zyanCoreStatusInsufficientBufferSize #-}

zyanCoreStatusNotEnoughMemory :: ZyanNativeStatus
zyanCoreStatusNotEnoughMemory = makeZyanNativeStatus True zyanModuleCore 0x0A
{-# INLINE zyanCoreStatusNotEnoughMemory #-}

zyanCoreStatusBadSystemCall :: ZyanNativeStatus
zyanCoreStatusBadSystemCall = makeZyanNativeStatus True zyanModuleCore 0x0B
{-# INLINE zyanCoreStatusBadSystemCall #-}

zyanCoreStatusOutOfResources :: ZyanNativeStatus
zyanCoreStatusOutOfResources = makeZyanNativeStatus True zyanModuleCore 0x0C
{-# INLINE zyanCoreStatusOutOfResources #-}

zyanCoreStatusMissingDependency :: ZyanNativeStatus
zyanCoreStatusMissingDependency = makeZyanNativeStatus True zyanModuleCore 0x0D
{-# INLINE zyanCoreStatusMissingDependency #-}

-- * Zydis status
zyanModuleZydis :: ZyanNativeModule
zyanModuleZydis = 0x002
{-# INLINE zyanModuleZydis #-}

zydisStatusNoMoreData :: ZyanNativeStatus
zydisStatusNoMoreData = makeZyanNativeStatus True zyanModuleZydis 0x00
{-# INLINE zydisStatusNoMoreData #-}

zydisStatusDecodingError :: ZyanNativeStatus
zydisStatusDecodingError = makeZyanNativeStatus True zyanModuleZydis 0x01
{-# INLINE zydisStatusDecodingError #-}

zydisStatusInstructionTooLong :: ZyanNativeStatus
zydisStatusInstructionTooLong = makeZyanNativeStatus True zyanModuleZydis 0x02
{-# INLINE zydisStatusInstructionTooLong #-}

zydisStatusBadRegister :: ZyanNativeStatus
zydisStatusBadRegister = makeZyanNativeStatus True zyanModuleZydis 0x03
{-# INLINE zydisStatusBadRegister #-}

zydisStatusIllegalLock :: ZyanNativeStatus
zydisStatusIllegalLock = makeZyanNativeStatus True zyanModuleZydis 0x04
{-# INLINE zydisStatusIllegalLock #-}

zydisStatusIllegalLegacyPFX :: ZyanNativeStatus
zydisStatusIllegalLegacyPFX = makeZyanNativeStatus True zyanModuleZydis 0x05
{-# INLINE zydisStatusIllegalLegacyPFX #-}

zydisStatusIllegalRex :: ZyanNativeStatus
zydisStatusIllegalRex = makeZyanNativeStatus True zyanModuleZydis 0x06
{-# INLINE zydisStatusIllegalRex #-}

zydisStatusInvalidMap :: ZyanNativeStatus
zydisStatusInvalidMap = makeZyanNativeStatus True zyanModuleZydis 0x07
{-# INLINE zydisStatusInvalidMap #-}

zydisStatusMalformedEvex :: ZyanNativeStatus
zydisStatusMalformedEvex = makeZyanNativeStatus True zyanModuleZydis 0x08
{-# INLINE zydisStatusMalformedEvex #-}

zydisStatusMalformedMvex :: ZyanNativeStatus
zydisStatusMalformedMvex = makeZyanNativeStatus True zyanModuleZydis 0x09
{-# INLINE zydisStatusMalformedMvex #-}

zydisStatusInvalidMask :: ZyanNativeStatus
zydisStatusInvalidMask = makeZyanNativeStatus True zyanModuleZydis 0x0A
{-# INLINE zydisStatusInvalidMask #-}

zydisStatusSkipToken :: ZyanNativeStatus
zydisStatusSkipToken = makeZyanNativeStatus False zyanModuleZydis 0x0B
{-# INLINE zydisStatusSkipToken #-}

-- | Marshal low level Zyan/Zydis status to our "ZyanStatus".
--
-- This function should cover the complete range of possibilities.
--
fromZyanNativeStatus :: ZyanNativeStatus -> ZyanStatus
fromZyanNativeStatus s
  | s == zyanCoreStatusSuccess = Left ZyanCoreStatusSuccess
  | s == zyanCoreStatusFailed = Left ZyanCoreStatusFailed
  | s == zyanCoreStatusTrue = Left ZyanCoreStatusTrue
  | s == zyanCoreStatusFalse = Left ZyanCoreStatusFalse
  | s == zyanCoreStatusInvalidArgument = Left ZyanCoreStatusInvalidArgument
  | s == zyanCoreStatusInvalidOperation = Left ZyanCoreStatusInvalidOperation
  | s == zyanCoreStatusAccessDenied = Left ZyanCoreStatusAccessDenied
  | s == zyanCoreStatusNotFound = Left ZyanCoreStatusNotFound
  | s == zyanCoreStatusOutOfRange = Left ZyanCoreStatusOutOfRange
  | s == zyanCoreStatusInsufficientBufferSize = Left
    ZyanCoreStatusInsufficientBufferSize
  | s == zyanCoreStatusNotEnoughMemory = Left ZyanCoreStatusNotEnoughMemory
  | s == zyanCoreStatusBadSystemCall = Left ZyanCoreStatusBadSystemCall
  | s == zyanCoreStatusOutOfResources = Left ZyanCoreStatusOutOfResources
  | s == zyanCoreStatusMissingDependency = Left ZyanCoreStatusMissingDependency
  | s == zydisStatusNoMoreData = Right ZydisStatusNoMoreData
  | s == zydisStatusInvalidMask = Right ZydisStatusInvalidMask
  | s == zydisStatusMalformedMvex = Right ZydisStatusMalformedMvex
  | s == zydisStatusMalformedEvex = Right ZydisStatusMalformedEvex
  | s == zydisStatusInvalidMap = Right ZydisStatusInvalidMap
  | s == zydisStatusIllegalRex = Right ZydisStatusIllegalRex
  | s == zydisStatusIllegalLegacyPFX = Right ZydisStatusIllegalLegacyPFX
  | s == zydisStatusIllegalLock = Right ZydisStatusIllegalLock
  | s == zydisStatusBadRegister = Right ZydisStatusBadRegister
  | s == zydisStatusInstructionTooLong = Right ZydisStatusInstructionTooLong
  | s == zydisStatusDecodingError = Right ZydisStatusDecodingError
  | s == zydisStatusSkipToken = Right ZydisStatusSkipToken
  | otherwise = error $ "Fatal error, missing zyan status code: " <> show s
{-# INLINE fromZyanNativeStatus #-}
