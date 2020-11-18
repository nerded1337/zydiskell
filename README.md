# Zydiskell

Haskell langage binding for the [Zydis library](https://github.com/zyantific/zydis), a fast and lightweight x86/x86-64 disassembler.

## Building

- Recursively clone the project: `git clone --recursive https://github.com/nerded1337/zydiskell`
- Either use Stack or Cabal: `stack build` | `cabal v2-build`

Note: The [Zydis library](https://github.com/zyantific/zydis) will be directly compiled by GHC.

## Interface

We currently expose three functions:

```haskell
import qualified Zydis as Z

Z.initialize :: MachineMode -> AddressWidth -> IO (Either ZyanStatus Decoder)

Z.decodeBuffer
  :: Decoder
  -> ByteString
  -> Offset
  -> Length
  -> IO (Either ZyanStatus DecodedInstruction)

Z. decodeFullBuffer
  :: Decoder -> ByteString -> IO (Either ZyanStatus (Vector DecodedInstruction))
```

## Example
```haskell
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
```
