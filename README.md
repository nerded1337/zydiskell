[![build status](https://github.com/nerded1337/zydiskell/workflows/build/badge.svg)](https://github.com/nerded1337/zydiskell/actions)
[![hackage version](https://img.shields.io/hackage/v/zydiskell)](https://hackage.haskell.org/package/zydiskell)

# Zydiskell

Haskell langage wrapper for the [Zydis library](https://github.com/zyantific/zydis), a fast and lightweight x86/x86-64 disassembler.

## Building

- Recursively clone the project: `git clone --recursive https://github.com/nerded1337/zydiskell`
- Either use Stack or Cabal: `stack build` | `cabal v2-build`

Notes:
- the [Zydis library](https://github.com/zyantific/zydis) is directly embedded and compiled by GHC.
- we support the last three major GHC versions, currently: `8.6`, `8.8` and `8.10`

## Interface

We currently expose three functions:

```haskell
import Data.ByteString (ByteString)
import qualified Zydis as Z

Z.initialize :: Z.MachineMode -> Z.AddressWidth -> IO (Either Z.ZyanStatus Z.Decoder)

Z.decodeBuffer
  :: Z.Decoder
  -> ByteString
  -> Z.Offset
  -> Z.Length
  -> IO (Either Z.ZyanStatus Z.DecodedInstruction)

Z.decodeFullBuffer
  :: Z.Decoder -> ByteString -> IO (Either Z.ZyanStatus (Seq Z.DecodedInstruction))
```

## Example
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Sequence
import           Data.Bifoldable
import qualified Zydis                         as Z

main :: IO ()
main = test

test :: IO ()
test = bitraverse_ initFailure decode =<< initZydis
 where
  zyanError :: Show a => String -> a -> IO ()
  zyanError s = putStrLn . ((s <> ". ZyanStatus: ") <>) . show

  initFailure :: Z.ZyanStatus -> IO ()
  initFailure = zyanError "Failed to initialize decoder"

  initZydis :: IO (Either Z.ZyanStatus Z.Decoder)
  initZydis = Z.initialize Z.MachineModeLong64 Z.AddressWidth64

  {-
     mov rax, 0xCAFEBABECAFEBABE
     push rax
     ret
  -}
  buffer    = "\x48\xB8\xBE\xBA\xFE\xCA\xBE\xBA\xFE\xCA\x50\xC3"

  decodingFailure :: Z.ZyanStatus -> IO ()
  decodingFailure = zyanError "Failed to decode buffer"

  {-
      Given the decoded buffer, should output: [MnemonicMov,MnemonicPush,MnemonicRet]
  -}
  printMnemonics :: Seq Z.DecodedInstruction -> IO ()
  printMnemonics = print . fmap Z.decodedInstructionMnemonic

  decode :: Z.Decoder -> IO ()
  decode decoder =
    bitraverse_ decodingFailure printMnemonics
      =<< Z.decodeFullBuffer decoder buffer
```
