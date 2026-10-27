# Non-empty Vectors

![Build Status](https://github.com/emilypi/nonempty-vector/actions/workflows/haskell-ci.yml/badge.svg) [![Hackage](https://img.shields.io/hackage/v/nonempty-vector.svg)](https://hackage.haskell.org/package/nonempty-vector)

This package provides thin wrappers around mutable and immutable
[vector](https://hackage.haskell.org/package/vector) types that enforce
non-emptiness, with APIs for both boxed and unboxed storage.

| Storage | Immutable | Mutable |
| --- | --- | --- |
| Boxed | `Data.Vector.NonEmpty` | `Data.Vector.NonEmpty.Mutable` |
| Unboxed | `Data.Vector.NonEmpty.Unboxed` | `Data.Vector.NonEmpty.Unboxed.Mutable` |

The unboxed API requires `Unbox` instances for element types. Operations that can
produce an empty result return an ordinary vector or `Maybe`, as appropriate.
The `Internal` modules expose constructors for users who manage the non-emptiness
invariant themselves.

Library dependencies are `base`, `deepseq`, `primitive`, and `vector`.
