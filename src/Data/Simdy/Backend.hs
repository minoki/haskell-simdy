-- |
-- Query information about the active SIMD backend.
--
-- The concrete backend is selected at compile time based on architecture and
-- cabal flags:
--
-- * @no-simd@ &#x2014; Scalar fallback. Always available.
-- * @vl128@ &#x2014; 128-bit vectors (SSE on x86_64, NEON on AArch64).
--
--     * On x86_64: requires the @llvm@ flag, or GHC >= 9.12 with the native code generator.
--     * On AArch64: requires GHC >= 9.12 with the @llvm@ flag (NEON support was added in
--       GHC 9.8 but is unusable on 9.8 and 9.10 due to compiler bugs),
--       or GHC >= 10.0 with the native code generator.
--     * Note: on GHC 9.12, the x86_64 NCG path only uses SIMD instructions for
--       'Float' and 'Double'; integer element types fall back to scalar operations.
--       GHC 9.14 extends NCG support to 128-bit integer vectors as well.
--
-- * @vl256@ &#x2014; 256-bit vectors (AVX2). Requires the @haswell@ or @avx512@ flag and the @llvm@ flag. x86_64 only.
-- * @vl512@ &#x2014; 512-bit vectors (AVX-512). Requires the @avx512@ flag and the @llvm@ flag. x86_64 only.
--
-- Use 'implementationDescription' to obtain a human-readable string describing
-- the active backend at runtime (e.g. @\"X8;maxBits=128\"@).
module Data.Simdy.Backend
  ( ImplementationDescription (..)
  ) where
import           Data.Simdy.Internal.Class
