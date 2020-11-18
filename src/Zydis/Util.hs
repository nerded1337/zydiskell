-- Util.hs ---

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

{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- |
-- = This handy module extend Storable typeclasse with default instances for C-like enums/fixed arrays (FFI).
--
-- Using 'StorableExt', we are now able to use deriving via clause on sum types.
--
-- @
-- data X
--   = A
--   | B
--   | C
--   deriving stock Enum
--   deriving Storable via StorableExt X
-- @
--
-- This type will be stored as a word32 (C enum FFI).
--
-- Using the 'StorableFixedArray', we are now able to encode fixed sizes in the type (in conjunction with storable-record "Foreign.Storable.FixedArray").
--
-- @
-- data X = X (StorableFixedArray Word32 10)
-- @
--
-- This type will be stored as 10 contiguous word32 (C fixed array).
--
module Zydis.Util
  ( StorableExt(..)
  , StorableFixedArray(..)
  , Storable
  )
where

import           Data.Foldable
import           Data.Proxy
import           Data.Vector
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import qualified Foreign.Storable.FixedArray   as Fixed
import           GHC.TypeLits


-- | Wrapper to extend storable default instances.
newtype StorableExt a =
  StorableExt
    { unStorableExt :: a
    }
  deriving stock (Show, Eq)

instance forall a. Enum a => Storable (StorableExt a) where
  alignment = const $ alignment @Word32 undefined
  sizeOf = const $ sizeOf @Word32 undefined
  peek = fmap (StorableExt . toEnum . fromIntegral) . peek . castPtr @_ @Word32
  poke ptr v =
    poke (castPtr @_ @Word32 ptr) (fromIntegral $ fromEnum $ unStorableExt v)

-- | Wrapper to extend storable default instances.
newtype StorableFixedArray a b =
  StorableFixedArray
    { unStorableFixedArray :: Vector a
    }
  deriving stock (Show, Eq)

instance forall a b. (Storable a, KnownNat b) => Storable (StorableFixedArray a b) where
  alignment = const $ alignment @a undefined
  sizeOf =
    const $ Fixed.sizeOfArray @a (fromIntegral $ natVal (Proxy @b)) undefined
  peek ptr = StorableFixedArray <$> Fixed.run ptr' loop
   where
    ptr' = castPtr ptr
    loop = replicateM (fromIntegral $ natVal (Proxy @b)) Fixed.peekNext
  poke ptr x = Fixed.run ptr' $ loop $ unStorableFixedArray x
   where
    ptr' = castPtr ptr
    loop = traverse_ Fixed.pokeNext
