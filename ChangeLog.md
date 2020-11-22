[0.2.0.0] - Unreleased

  * **Note**: breaking change.
    * `Zydis.Decoder.decodeFullBuffer` now return a `Seq` instead of a `Vector`.
    * introduce `ZyanStatus`, `ZyanCoreStatus` and `ZydisStatus` in `Zydis.Status` which are directly mapped from their C counterparts.

  * remove `vector` dependency
  * introduce `containers` dependency

[0.1.1.0] - November 2020

  * initial release
