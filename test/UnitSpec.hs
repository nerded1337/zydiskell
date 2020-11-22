-- UnitSpec.hs ---

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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module UnitSpec
  ( spec
  )
where

import           Data.ByteString               as BS
import           Data.Either
import           Data.Monoid
import           Foreign.Storable
import           Test.Hspec
import qualified Zydis                         as Z

initZydis :: IO (Either Z.ZyanStatus Z.Decoder)
initZydis = Z.initialize Z.MachineModeLong64 Z.AddressWidth64

spec :: Spec
spec = do
  describe "Type storable marshalling" $ do

    it "should match the underlying C type size" $ do

      -- This constant is extracted from ZydisInfo which is located in external/zydis/tools
      -- It's a simple sizeof(ZydisDecodedInstruction)
      let sizeOfDecodedInstruction = 1472
      sizeOf @Z.DecodedInstruction undefined `shouldBe` sizeOfDecodedInstruction

  describe "Decoder" $ do

    it "should initialize" $ do
      initZydis >>= (`shouldSatisfy` isRight)

    describe "and then" $ do

      -- Assuming we reach this code, it's "impossible" to branch on the Left
      d <- runIO $ fromRight (error "impossible") <$> initZydis

      it "should decode a single instruction" $ do
        let instructions =
              [ "\x9C"
              , "\x9D"
              , "\x9E"
              , "\x9F"
              , "\xF8"
              , "\xF9"
              , "\xFA"
              , "\xFB"
              , "\xFC"
              , "\xC3"
              , "\xCC"
              , "\x0F\x08"
              , "\x48\xB8\xBE\xBA\xFE\xCA\xBE\xBA\xFE\xCA"
              ]
        traverse (\i -> Z.decodeBuffer d i 0 (fromIntegral $ BS.length i))
                 instructions
          >>= (`shouldSatisfy` getAll . foldMap (All . isRight))

      it "should fail on partial input" $ do
        let truncatedInstructions = ["\x0F", "\x48\xB8\xBE\xBA\xFE\xCA"]
        traverse (\i -> Z.decodeBuffer d i 0 (fromIntegral $ BS.length i))
                 truncatedInstructions
          >>= (`shouldSatisfy` getAll
                . foldMap (All . (== Left (Right Z.ZydisStatusNoMoreData)))
              )

