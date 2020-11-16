-- Main.hs ---

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

module Main where

import           Data.Bitraversable
import           Data.Bifoldable
import qualified Zydis                         as Z

main :: IO ()
main = test

test :: IO ()
test = bitraverse_ initFailure decode =<< initZydis
 where
  zyanError s = putStrLn . ((s <> ". ZyanStatus: ") <>) . show

  initFailure     = zyanError "Failed to initialize decoder"

  initZydis       = Z.initialize Z.MachineModeLong64 Z.AddressWidth64

  {-
     mov rax, 0xCAFEBABECAFEBABE
     push rax
     ret
  -}
  buffer          = "\x48\xB8\xBE\xBA\xFE\xCA\xBE\xBA\xFE\xCA\x50\xC3"

  decodingFailure = zyanError "Failed to decode buffer"

  {-
      Given the decoded buffer, should output: [MnemonicMov,MnemonicPush,MnemonicRet]
  -}
  printMnemonics  = print . fmap Z.decodedInstructionMnemonic

  decode decoder =
    bitraverse decodingFailure printMnemonics
      =<< Z.decodeFullBuffer decoder buffer
