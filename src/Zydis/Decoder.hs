-- Decoder.hs ---

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

{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications         #-}

module Zydis.Decoder
  ( ZyanStatus
  , ZyanUSize
  , Offset
  , Length
  , initialize
  , decodeBuffer
  , decodeFullBuffer
  )
where

import           Data.Bits
import           Data.ByteString               as BS
import           Data.ByteString.Internal
import           Data.Vector
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           Zydis.Types

-- * FFI types

type MachineModeC = Word32

type AddressWidthC = Word32

type ZyanStatus = Word32

type ZyanUSize = Word64

type Offset = ZyanUSize

type Length = ZyanUSize

-- * FFI declarations

foreign import ccall unsafe "ZydisDecoderInit" c_ZydisDecoderInit
  :: Ptr Decoder -> MachineModeC -> AddressWidthC -> IO ZyanStatus

foreign import ccall unsafe "ZydisDecoderDecodeBuffer" c_ZydisDecoderDecodeBuffer
  :: Ptr Decoder -> Ptr Word8 -> ZyanUSize -> Ptr DecodedInstruction -> IO ZyanStatus

-- * FFI bridges

-- | Directly stolen from https://github.com/zyantific/zycore-c/blob/71440fa634d1313db735d3262d453be641bb404f/include/Zycore/Status.h#L81
zyanSuccess :: Word32 -> Bool
zyanSuccess x = (x .&. 0x80000000) == 0
{-# INLINE zyanSuccess #-}

-- | Initialize a Zydis decoder, required to decode instructions.
initialize :: MachineMode -> AddressWidth -> IO (Either ZyanStatus Decoder)
initialize mm aw = alloca go
 where
  go decoder = do
    r <- c_ZydisDecoderInit decoder
                            (fromIntegral $ fromEnum mm)
                            (fromIntegral $ fromEnum aw)
    if zyanSuccess r then Right <$> peek decoder else pure $ Left r
{-# INLINE initialize #-}

-- | Decode a single intruction.
decodeBuffer
  :: Decoder
  -> ByteString
  -> Offset
  -> Length
  -> IO (Either ZyanStatus DecodedInstruction)
decodeBuffer d bs o l = alloca @Decoder go
 where
  (bufferForeignPtr, _, _) = toForeignPtr bs

  go decoderPtr = alloca @DecodedInstruction $ go' decoderPtr

  go' decoderPtr decodedInstructionPtr =
    withForeignPtr bufferForeignPtr $ go'' decoderPtr decodedInstructionPtr

  go'' decoderPtr decodedInstructionPtr bufferPtr = do
    poke decoderPtr d
    doDecodeInstruction decoderPtr decodedInstructionPtr bufferPtr o l
{-# INLINE decodeBuffer #-}

-- | Efficiently decode an entire buffer of instructions.
decodeFullBuffer
  :: Decoder -> ByteString -> IO (Either ZyanStatus (Vector DecodedInstruction))
decodeFullBuffer d bs = alloca @Decoder go
 where
  (bufferForeignPtr, _, bufferLength) = toForeignPtr bs

  go decoderPtr = alloca @DecodedInstruction $ go' decoderPtr

  go' decoderPtr decodedInstructionPtr =
    withForeignPtr bufferForeignPtr $ go'' decoderPtr decodedInstructionPtr

  go'' decoderPtr decodedInstructionPtr bufferPtr = do
    poke decoderPtr d
    loop (mempty @(Vector DecodedInstruction), 0, fromIntegral bufferLength)
   where
    loop (!v, !o, !l)
      | l > 0 = do
        x <- doDecodeInstruction decoderPtr decodedInstructionPtr bufferPtr o l
        case x of
          Right i -> do
            let il = fromIntegral $ decodedInstructionLength i
            loop (v <> pure i, o + il, l - il)
          Left s -> pure $ Left s
      | otherwise = pure $ Right v
{-# INLINE decodeFullBuffer #-}

doDecodeInstruction
  :: Ptr Decoder
  -> Ptr DecodedInstruction
  -> Ptr Word8
  -> Offset
  -> Length
  -> IO (Either ZyanStatus DecodedInstruction)
doDecodeInstruction decoderPtr decodedInstructionPtr bufferPtr o l = do
  r <- c_ZydisDecoderDecodeBuffer decoderPtr
                                  (plusPtr bufferPtr (fromIntegral o))
                                  l
                                  decodedInstructionPtr
  if zyanSuccess r then Right <$> peek decodedInstructionPtr else pure $ Left r
{-# INLINE doDecodeInstruction #-}
