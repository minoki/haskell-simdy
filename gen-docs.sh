#!/bin/sh
set -eu -o pipefail
mkdir -p docs/simdy
cabal haddock \
  --haddock-hyperlink-source \
  --haddock-option=--use-index=../doc-index.html \
  --haddock-option=--use-contents=../index.html \
  --haddock-option=--base-url=.. \
  --haddock-option=--hide=Data.Simdy.Internal.Bits \
  --haddock-option=--hide=Data.Simdy.Internal.FMA \
  --haddock-option=--hide=Data.Simdy.Internal.PrimExtra \
  --haddock-option=--hide=Data.Simdy.Internal.Shuffle \
  --haddock-output-dir=docs/common \
  lib:common
cabal haddock \
  --haddock-hyperlink-source \
  --haddock-option=--use-index=../doc-index.html \
  --haddock-option=--use-contents=../index.html \
  --haddock-option=--base-url=.. \
  --haddock-option=--read-interface=../common,../common/src,docs/common/simdy.haddock \
  --haddock-output-dir=docs/no-simd \
  lib:no-simd
cabal haddock \
  --haddock-hyperlink-source \
  --haddock-option=--use-index=../doc-index.html \
  --haddock-option=--use-contents=../index.html \
  --haddock-option=--base-url=.. \
  --haddock-option=--hide=Data.Simdy.Internal.SIMD128.Prim \
  --haddock-option=--hide=Data.Simdy.Internal.SIMD128.PrimExtra \
  --haddock-option=--read-interface=../common,../common/src,docs/common/simdy.haddock \
  --haddock-output-dir=docs/vl128 \
  lib:vl128
cabal haddock \
  --haddock-hyperlink-source \
  --haddock-option=--use-index=../doc-index.html \
  --haddock-option=--use-contents=../index.html \
  --haddock-option=--base-url=.. \
  --haddock-output-dir=docs/plugin \
  lib:plugin
cabal haddock \
  --haddock-hyperlink-source \
  --haddock-option=--use-index=../doc-index.html \
  --haddock-option=--use-contents=../index.html \
  --haddock-option=--base-url=.. \
  --haddock-option=--read-interface=../common,../common/src,docs/common/simdy.haddock \
  --haddock-option=--read-interface=../no-simd,../no-simd/src,docs/no-simd/simdy.haddock \
  --haddock-option=--read-interface=../vl128,../vl128/src,docs/vl128/simdy.haddock \
  --haddock-output-dir=docs/simdy \
  simdy
haddock -o docs --quickjump --gen-index --gen-contents \
  --read-interface=simdy,simdy/src,docs/simdy/simdy.haddock \
  --read-interface=common,common/src,docs/common/simdy.haddock \
  --read-interface=no-simd,no-simd/src,docs/no-simd/simdy.haddock \
  --read-interface=vl128,vl128/src,docs/vl128/simdy.haddock \
  --read-interface=plugin,plugin/src,docs/plugin/simdy.haddock \
