{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Simdy.Shuffle.Plugin (plugin) where
import           Control.Applicative ((<|>))
import           Control.Monad (forM, guard, replicateM)
import           Data.Either (partitionEithers)
import           Data.List (nub)
import           Data.Maybe (fromMaybe)
import           Data.Simdy.Internal.Shuffle (Pick, ShuffleMany)
import qualified GHC.Builtin.Types as B
import qualified GHC.Builtin.Types.Prim as B
import           GHC.Core.Make (mkCoreApps, mkNaturalExpr)
import           GHC.Core.Type (RuntimeRepType)
import           GHC.Driver.Backend (DefunctionalizedCodeOutput (LlvmCodeOutput, NcgCodeOutput),
                                     backendCodeOutput)
import           GHC.Driver.DynFlags (DynFlags (backend), isAvxEnabled)
import           GHC.Plugins (getDynFlags, targetPlatform)
import qualified GHC.Plugins as GHC (Alt (Alt), AltCon (DataAlt),
                                     Boxity (Unboxed),
                                     Expr (App, Case, Lam, Lit, Type, Var),
                                     Plugin (..), defaultPlugin, getUniqueM,
                                     intRepDataConTy, manyDataConTy,
                                     mkCoreConApps, mkLitInt, mkLocalId,
                                     mkSystemName, mkTyConApp, purePlugin,
                                     tupleDataCon)
import           GHC.TcPlugin.API
import           GHC.TcPlugin.API.Internal (unsafeLiftTcM)
import           GHC.Types.Literal (mkLitDouble, mkLitFloat, mkLitInt16,
                                    mkLitInt32, mkLitInt64, mkLitInt8,
                                    mkLitWord16, mkLitWord32, mkLitWord64,
                                    mkLitWord8)
-- import GHC.Utils.Outputable -- for debugging
#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)
import           GHC.PrimOps
#else
import           GHC.Exts
#endif

#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0)
import           GHC.Tc.Types.Evidence (evUnaryDictAppE)
#else
evUnaryDictAppE :: Class -> [Type] -> EvExpr -> EvExpr
evUnaryDictAppE cls tys meth = case tyConSingleDataCon_maybe (classTyCon cls) of
  Just dc -> evDataConApp dc tys [meth]
  Nothing -> pprPanic "evUnaryDictAppE" (ppr cls)
#endif

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
  { GHC.tcPlugin = \_args -> Just $ mkTcPlugin tcPlugin
  , GHC.pluginRecompile = GHC.purePlugin
  }

tcPlugin :: TcPlugin
tcPlugin = TcPlugin
  { tcPluginInit = pluginInit
  , tcPluginSolve = pluginSolve
  , tcPluginRewrite = const emptyUFM
  , tcPluginStop = \_ -> pure ()
  }

data VectorTypeDefs = MkVectorTypeDefs
  { numberOfElements   :: !Int
  , tyCon              :: TyCon
  , scalarTyCon        :: TyCon
  , scalarRepDataConTy :: RuntimeRepType
  , shuffleId          :: Maybe Id
  , broadcastId        :: Id
  , packId             :: Id
  , unpackId           :: Id
  , zeroExpr           :: CoreExpr
  }

data PluginDefs = MkPluginDefs
  { shuffleManyClass :: Class
  , pickClass        :: Class
  {-
  , promotedNothingDataCon :: TyCon
  , promotedJustDataCon :: TyCon
  , promotedLeftDataCon :: TyCon
  , promotedRightDataCon :: TyCon
  -}
  , table            :: [(TyCon, VectorTypeDefs)]
  }

pluginInit :: TcPluginM Init PluginDefs
pluginInit = do
  shuffleManyClass <- lookupTHName ''ShuffleMany >>= tcLookupClass
  pickClass <- lookupTHName ''Pick >>= tcLookupClass
#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)
  shuffleFloatX4Prim <- lookupTHName 'shuffleFloatX4# >>= tcLookupId
  shuffleDoubleX2Prim <- lookupTHName 'shuffleDoubleX2# >>= tcLookupId
  shuffleInt8X16Prim <- lookupTHName 'shuffleInt8X16# >>= tcLookupId
  shuffleInt16X8Prim <- lookupTHName 'shuffleInt16X8# >>= tcLookupId
  shuffleInt32X4Prim <- lookupTHName 'shuffleInt32X4# >>= tcLookupId
  shuffleInt64X2Prim <- lookupTHName 'shuffleInt64X2# >>= tcLookupId
  shuffleWord8X16Prim <- lookupTHName 'shuffleWord8X16# >>= tcLookupId
  shuffleWord16X8Prim <- lookupTHName 'shuffleWord16X8# >>= tcLookupId
  shuffleWord32X4Prim <- lookupTHName 'shuffleWord32X4# >>= tcLookupId
  shuffleWord64X2Prim <- lookupTHName 'shuffleWord64X2# >>= tcLookupId
  shuffleFloatX8Prim <- lookupTHName 'shuffleFloatX8# >>= tcLookupId
  shuffleDoubleX4Prim <- lookupTHName 'shuffleDoubleX4# >>= tcLookupId
  shuffleInt8X32Prim <- lookupTHName 'shuffleInt8X32# >>= tcLookupId
  shuffleInt16X16Prim <- lookupTHName 'shuffleInt16X16# >>= tcLookupId
  shuffleInt32X8Prim <- lookupTHName 'shuffleInt32X8# >>= tcLookupId
  shuffleInt64X4Prim <- lookupTHName 'shuffleInt64X4# >>= tcLookupId
  shuffleWord8X32Prim <- lookupTHName 'shuffleWord8X32# >>= tcLookupId
  shuffleWord16X16Prim <- lookupTHName 'shuffleWord16X16# >>= tcLookupId
  shuffleWord32X8Prim <- lookupTHName 'shuffleWord32X8# >>= tcLookupId
  shuffleWord64X4Prim <- lookupTHName 'shuffleWord64X4# >>= tcLookupId
  shuffleFloatX16Prim <- lookupTHName 'shuffleFloatX16# >>= tcLookupId
  shuffleDoubleX8Prim <- lookupTHName 'shuffleDoubleX8# >>= tcLookupId
  shuffleInt8X64Prim <- lookupTHName 'shuffleInt8X64# >>= tcLookupId
  shuffleInt16X32Prim <- lookupTHName 'shuffleInt16X32# >>= tcLookupId
  shuffleInt32X16Prim <- lookupTHName 'shuffleInt32X16# >>= tcLookupId
  shuffleInt64X8Prim <- lookupTHName 'shuffleInt64X8# >>= tcLookupId
  shuffleWord8X64Prim <- lookupTHName 'shuffleWord8X64# >>= tcLookupId
  shuffleWord16X32Prim <- lookupTHName 'shuffleWord16X32# >>= tcLookupId
  shuffleWord32X16Prim <- lookupTHName 'shuffleWord32X16# >>= tcLookupId
  shuffleWord64X8Prim <- lookupTHName 'shuffleWord64X8# >>= tcLookupId
#endif
  broadcastFloatX4Prim <- lookupTHName 'broadcastFloatX4# >>= tcLookupId
  broadcastDoubleX2Prim <- lookupTHName 'broadcastDoubleX2# >>= tcLookupId
  broadcastInt8X16Prim <- lookupTHName 'broadcastInt8X16# >>= tcLookupId
  broadcastInt16X8Prim <- lookupTHName 'broadcastInt16X8# >>= tcLookupId
  broadcastInt32X4Prim <- lookupTHName 'broadcastInt32X4# >>= tcLookupId
  broadcastInt64X2Prim <- lookupTHName 'broadcastInt64X2# >>= tcLookupId
  broadcastWord8X16Prim <- lookupTHName 'broadcastWord8X16# >>= tcLookupId
  broadcastWord16X8Prim <- lookupTHName 'broadcastWord16X8# >>= tcLookupId
  broadcastWord32X4Prim <- lookupTHName 'broadcastWord32X4# >>= tcLookupId
  broadcastWord64X2Prim <- lookupTHName 'broadcastWord64X2# >>= tcLookupId
  broadcastFloatX8Prim <- lookupTHName 'broadcastFloatX8# >>= tcLookupId
  broadcastDoubleX4Prim <- lookupTHName 'broadcastDoubleX4# >>= tcLookupId
  broadcastInt8X32Prim <- lookupTHName 'broadcastInt8X32# >>= tcLookupId
  broadcastInt16X16Prim <- lookupTHName 'broadcastInt16X16# >>= tcLookupId
  broadcastInt32X8Prim <- lookupTHName 'broadcastInt32X8# >>= tcLookupId
  broadcastInt64X4Prim <- lookupTHName 'broadcastInt64X4# >>= tcLookupId
  broadcastWord8X32Prim <- lookupTHName 'broadcastWord8X32# >>= tcLookupId
  broadcastWord16X16Prim <- lookupTHName 'broadcastWord16X16# >>= tcLookupId
  broadcastWord32X8Prim <- lookupTHName 'broadcastWord32X8# >>= tcLookupId
  broadcastWord64X4Prim <- lookupTHName 'broadcastWord64X4# >>= tcLookupId
  broadcastFloatX16Prim <- lookupTHName 'broadcastFloatX16# >>= tcLookupId
  broadcastDoubleX8Prim <- lookupTHName 'broadcastDoubleX8# >>= tcLookupId
  broadcastInt8X64Prim <- lookupTHName 'broadcastInt8X64# >>= tcLookupId
  broadcastInt16X32Prim <- lookupTHName 'broadcastInt16X32# >>= tcLookupId
  broadcastInt32X16Prim <- lookupTHName 'broadcastInt32X16# >>= tcLookupId
  broadcastInt64X8Prim <- lookupTHName 'broadcastInt64X8# >>= tcLookupId
  broadcastWord8X64Prim <- lookupTHName 'broadcastWord8X64# >>= tcLookupId
  broadcastWord16X32Prim <- lookupTHName 'broadcastWord16X32# >>= tcLookupId
  broadcastWord32X16Prim <- lookupTHName 'broadcastWord32X16# >>= tcLookupId
  broadcastWord64X8Prim <- lookupTHName 'broadcastWord64X8# >>= tcLookupId
  packFloatX4Prim <- lookupTHName 'packFloatX4# >>= tcLookupId
  packDoubleX2Prim <- lookupTHName 'packDoubleX2# >>= tcLookupId
  packInt8X16Prim <- lookupTHName 'packInt8X16# >>= tcLookupId
  packInt16X8Prim <- lookupTHName 'packInt16X8# >>= tcLookupId
  packInt32X4Prim <- lookupTHName 'packInt32X4# >>= tcLookupId
  packInt64X2Prim <- lookupTHName 'packInt64X2# >>= tcLookupId
  packWord8X16Prim <- lookupTHName 'packWord8X16# >>= tcLookupId
  packWord16X8Prim <- lookupTHName 'packWord16X8# >>= tcLookupId
  packWord32X4Prim <- lookupTHName 'packWord32X4# >>= tcLookupId
  packWord64X2Prim <- lookupTHName 'packWord64X2# >>= tcLookupId
  packFloatX8Prim <- lookupTHName 'packFloatX8# >>= tcLookupId
  packDoubleX4Prim <- lookupTHName 'packDoubleX4# >>= tcLookupId
  packInt8X32Prim <- lookupTHName 'packInt8X32# >>= tcLookupId
  packInt16X16Prim <- lookupTHName 'packInt16X16# >>= tcLookupId
  packInt32X8Prim <- lookupTHName 'packInt32X8# >>= tcLookupId
  packInt64X4Prim <- lookupTHName 'packInt64X4# >>= tcLookupId
  packWord8X32Prim <- lookupTHName 'packWord8X32# >>= tcLookupId
  packWord16X16Prim <- lookupTHName 'packWord16X16# >>= tcLookupId
  packWord32X8Prim <- lookupTHName 'packWord32X8# >>= tcLookupId
  packWord64X4Prim <- lookupTHName 'packWord64X4# >>= tcLookupId
  packFloatX16Prim <- lookupTHName 'packFloatX16# >>= tcLookupId
  packDoubleX8Prim <- lookupTHName 'packDoubleX8# >>= tcLookupId
  packInt8X64Prim <- lookupTHName 'packInt8X64# >>= tcLookupId
  packInt16X32Prim <- lookupTHName 'packInt16X32# >>= tcLookupId
  packInt32X16Prim <- lookupTHName 'packInt32X16# >>= tcLookupId
  packInt64X8Prim <- lookupTHName 'packInt64X8# >>= tcLookupId
  packWord8X64Prim <- lookupTHName 'packWord8X64# >>= tcLookupId
  packWord16X32Prim <- lookupTHName 'packWord16X32# >>= tcLookupId
  packWord32X16Prim <- lookupTHName 'packWord32X16# >>= tcLookupId
  packWord64X8Prim <- lookupTHName 'packWord64X8# >>= tcLookupId
  unpackFloatX4Prim <- lookupTHName 'unpackFloatX4# >>= tcLookupId
  unpackDoubleX2Prim <- lookupTHName 'unpackDoubleX2# >>= tcLookupId
  unpackInt8X16Prim <- lookupTHName 'unpackInt8X16# >>= tcLookupId
  unpackInt16X8Prim <- lookupTHName 'unpackInt16X8# >>= tcLookupId
  unpackInt32X4Prim <- lookupTHName 'unpackInt32X4# >>= tcLookupId
  unpackInt64X2Prim <- lookupTHName 'unpackInt64X2# >>= tcLookupId
  unpackWord8X16Prim <- lookupTHName 'unpackWord8X16# >>= tcLookupId
  unpackWord16X8Prim <- lookupTHName 'unpackWord16X8# >>= tcLookupId
  unpackWord32X4Prim <- lookupTHName 'unpackWord32X4# >>= tcLookupId
  unpackWord64X2Prim <- lookupTHName 'unpackWord64X2# >>= tcLookupId
  unpackFloatX8Prim <- lookupTHName 'unpackFloatX8# >>= tcLookupId
  unpackDoubleX4Prim <- lookupTHName 'unpackDoubleX4# >>= tcLookupId
  unpackInt8X32Prim <- lookupTHName 'unpackInt8X32# >>= tcLookupId
  unpackInt16X16Prim <- lookupTHName 'unpackInt16X16# >>= tcLookupId
  unpackInt32X8Prim <- lookupTHName 'unpackInt32X8# >>= tcLookupId
  unpackInt64X4Prim <- lookupTHName 'unpackInt64X4# >>= tcLookupId
  unpackWord8X32Prim <- lookupTHName 'unpackWord8X32# >>= tcLookupId
  unpackWord16X16Prim <- lookupTHName 'unpackWord16X16# >>= tcLookupId
  unpackWord32X8Prim <- lookupTHName 'unpackWord32X8# >>= tcLookupId
  unpackWord64X4Prim <- lookupTHName 'unpackWord64X4# >>= tcLookupId
  unpackFloatX16Prim <- lookupTHName 'unpackFloatX16# >>= tcLookupId
  unpackDoubleX8Prim <- lookupTHName 'unpackDoubleX8# >>= tcLookupId
  unpackInt8X64Prim <- lookupTHName 'unpackInt8X64# >>= tcLookupId
  unpackInt16X32Prim <- lookupTHName 'unpackInt16X32# >>= tcLookupId
  unpackInt32X16Prim <- lookupTHName 'unpackInt32X16# >>= tcLookupId
  unpackInt64X8Prim <- lookupTHName 'unpackInt64X8# >>= tcLookupId
  unpackWord8X64Prim <- lookupTHName 'unpackWord8X64# >>= tcLookupId
  unpackWord16X32Prim <- lookupTHName 'unpackWord16X32# >>= tcLookupId
  unpackWord32X16Prim <- lookupTHName 'unpackWord32X16# >>= tcLookupId
  unpackWord64X8Prim <- lookupTHName 'unpackWord64X8# >>= tcLookupId
  {-
  nothingDataCon <- lookupTHName 'Nothing >>= tcLookupDataCon
  let promotedNothingDataCon = promoteDataCon nothingDataCon
  justDataCon <- lookupTHName 'Just >>= tcLookupDataCon
  let promotedJustDataCon = promoteDataCon justDataCon
  leftDataCon <- lookupTHName 'Left >>= tcLookupDataCon
  let promotedLeftDataCon = promoteDataCon leftDataCon
  rightDataCon <- lookupTHName 'Right >>= tcLookupDataCon
  let promotedRightDataCon = promoteDataCon rightDataCon
  -}
#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)
  let table = [(B.floatX4PrimTyCon, MkVectorTypeDefs 4 B.floatX4PrimTyCon B.floatPrimTyCon B.floatRepDataConTy (Just shuffleFloatX4Prim) broadcastFloatX4Prim packFloatX4Prim unpackFloatX4Prim (GHC.Lit (mkLitFloat 0)))
              ,(B.doubleX2PrimTyCon, MkVectorTypeDefs 2 B.doubleX2PrimTyCon B.doublePrimTyCon B.doubleRepDataConTy (Just shuffleDoubleX2Prim) broadcastDoubleX2Prim packDoubleX2Prim unpackDoubleX2Prim (GHC.Lit (mkLitDouble 0)))
              ,(B.int8X16PrimTyCon, MkVectorTypeDefs 16 B.int8X16PrimTyCon B.int8PrimTyCon B.int8RepDataConTy (Just shuffleInt8X16Prim) broadcastInt8X16Prim packInt8X16Prim unpackInt8X16Prim (GHC.Lit (mkLitInt8 0)))
              ,(B.int16X8PrimTyCon, MkVectorTypeDefs 8 B.int16X8PrimTyCon B.int16PrimTyCon B.int16RepDataConTy (Just shuffleInt16X8Prim) broadcastInt16X8Prim packInt16X8Prim unpackInt16X8Prim (GHC.Lit (mkLitInt16 0)))
              ,(B.int32X4PrimTyCon, MkVectorTypeDefs 4 B.int32X4PrimTyCon B.int32PrimTyCon B.int32RepDataConTy (Just shuffleInt32X4Prim) broadcastInt32X4Prim packInt32X4Prim unpackInt32X4Prim (GHC.Lit (mkLitInt32 0)))
              ,(B.int64X2PrimTyCon, MkVectorTypeDefs 2 B.int64X2PrimTyCon B.int64PrimTyCon B.int64RepDataConTy (Just shuffleInt64X2Prim) broadcastInt64X2Prim packInt64X2Prim unpackInt64X2Prim (GHC.Lit (mkLitInt64 0)))
              ,(B.word8X16PrimTyCon, MkVectorTypeDefs 16 B.word8X16PrimTyCon B.word8PrimTyCon B.word8RepDataConTy (Just shuffleWord8X16Prim) broadcastWord8X16Prim packWord8X16Prim unpackWord8X16Prim (GHC.Lit (mkLitWord8 0)))
              ,(B.word16X8PrimTyCon, MkVectorTypeDefs 8 B.word16X8PrimTyCon B.word16PrimTyCon B.word16RepDataConTy (Just shuffleWord16X8Prim) broadcastWord16X8Prim packWord16X8Prim unpackWord16X8Prim (GHC.Lit (mkLitWord16 0)))
              ,(B.word32X4PrimTyCon, MkVectorTypeDefs 4 B.word32X4PrimTyCon B.word32PrimTyCon B.word32RepDataConTy (Just shuffleWord32X4Prim) broadcastWord32X4Prim packWord32X4Prim unpackWord32X4Prim (GHC.Lit (mkLitWord32 0)))
              ,(B.word64X2PrimTyCon, MkVectorTypeDefs 2 B.word64X2PrimTyCon B.word64PrimTyCon B.word64RepDataConTy (Just shuffleWord64X2Prim) broadcastWord64X2Prim packWord64X2Prim unpackWord64X2Prim (GHC.Lit (mkLitWord64 0)))

              ,(B.floatX8PrimTyCon, MkVectorTypeDefs 8 B.floatX8PrimTyCon B.floatPrimTyCon B.floatRepDataConTy (Just shuffleFloatX8Prim) broadcastFloatX8Prim packFloatX8Prim unpackFloatX8Prim (GHC.Lit (mkLitFloat 0)))
              ,(B.doubleX4PrimTyCon, MkVectorTypeDefs 4 B.doubleX4PrimTyCon B.doublePrimTyCon B.doubleRepDataConTy (Just shuffleDoubleX4Prim) broadcastDoubleX4Prim packDoubleX4Prim unpackDoubleX4Prim (GHC.Lit (mkLitDouble 0)))
              ,(B.int8X32PrimTyCon, MkVectorTypeDefs 32 B.int8X32PrimTyCon B.int8PrimTyCon B.int8RepDataConTy (Just shuffleInt8X32Prim) broadcastInt8X32Prim packInt8X32Prim unpackInt8X32Prim (GHC.Lit (mkLitInt8 0)))
              ,(B.int16X16PrimTyCon, MkVectorTypeDefs 16 B.int16X16PrimTyCon B.int16PrimTyCon B.int16RepDataConTy (Just shuffleInt16X16Prim) broadcastInt16X16Prim packInt16X16Prim unpackInt16X16Prim (GHC.Lit (mkLitInt16 0)))
              ,(B.int32X8PrimTyCon, MkVectorTypeDefs 8 B.int32X8PrimTyCon B.int32PrimTyCon B.int32RepDataConTy (Just shuffleInt32X8Prim) broadcastInt32X8Prim packInt32X8Prim unpackInt32X8Prim (GHC.Lit (mkLitInt32 0)))
              ,(B.int64X4PrimTyCon, MkVectorTypeDefs 4 B.int64X4PrimTyCon B.int64PrimTyCon B.int64RepDataConTy (Just shuffleInt64X4Prim) broadcastInt64X4Prim packInt64X4Prim unpackInt64X4Prim (GHC.Lit (mkLitInt64 0)))
              ,(B.word8X32PrimTyCon, MkVectorTypeDefs 32 B.word8X32PrimTyCon B.word8PrimTyCon B.word8RepDataConTy (Just shuffleWord8X32Prim) broadcastWord8X32Prim packWord8X32Prim unpackWord8X32Prim (GHC.Lit (mkLitWord8 0)))
              ,(B.word16X16PrimTyCon, MkVectorTypeDefs 16 B.word16X16PrimTyCon B.word16PrimTyCon B.word16RepDataConTy (Just shuffleWord16X16Prim) broadcastWord16X16Prim packWord16X16Prim unpackWord16X16Prim (GHC.Lit (mkLitWord16 0)))
              ,(B.word32X8PrimTyCon, MkVectorTypeDefs 8 B.word32X8PrimTyCon B.word32PrimTyCon B.word32RepDataConTy (Just shuffleWord32X8Prim) broadcastWord32X8Prim packWord32X8Prim unpackWord32X8Prim (GHC.Lit (mkLitWord32 0)))
              ,(B.word64X4PrimTyCon, MkVectorTypeDefs 4 B.word64X4PrimTyCon B.word64PrimTyCon B.word64RepDataConTy (Just shuffleWord64X4Prim) broadcastWord64X4Prim packWord64X4Prim unpackWord64X4Prim (GHC.Lit (mkLitWord64 0)))

              ,(B.floatX16PrimTyCon, MkVectorTypeDefs 16 B.floatX16PrimTyCon B.floatPrimTyCon B.floatRepDataConTy (Just shuffleFloatX16Prim) broadcastFloatX16Prim packFloatX16Prim unpackFloatX16Prim (GHC.Lit (mkLitFloat 0)))
              ,(B.doubleX8PrimTyCon, MkVectorTypeDefs 8 B.doubleX8PrimTyCon B.doublePrimTyCon B.doubleRepDataConTy (Just shuffleDoubleX8Prim) broadcastDoubleX8Prim packDoubleX8Prim unpackDoubleX8Prim (GHC.Lit (mkLitDouble 0)))
              ,(B.int8X64PrimTyCon, MkVectorTypeDefs 64 B.int8X64PrimTyCon B.int8PrimTyCon B.int8RepDataConTy (Just shuffleInt8X64Prim) broadcastInt8X64Prim packInt8X64Prim unpackInt8X64Prim (GHC.Lit (mkLitInt8 0)))
              ,(B.int16X32PrimTyCon, MkVectorTypeDefs 32 B.int16X32PrimTyCon B.int16PrimTyCon B.int16RepDataConTy (Just shuffleInt16X32Prim) broadcastInt16X32Prim packInt16X32Prim unpackInt16X32Prim (GHC.Lit (mkLitInt16 0)))
              ,(B.int32X16PrimTyCon, MkVectorTypeDefs 16 B.int32X16PrimTyCon B.int32PrimTyCon B.int32RepDataConTy (Just shuffleInt32X16Prim) broadcastInt32X16Prim packInt32X16Prim unpackInt32X16Prim (GHC.Lit (mkLitInt32 0)))
              ,(B.int64X8PrimTyCon, MkVectorTypeDefs 8 B.int64X8PrimTyCon B.int64PrimTyCon B.int64RepDataConTy (Just shuffleInt64X8Prim) broadcastInt64X8Prim packInt64X8Prim unpackInt64X8Prim (GHC.Lit (mkLitInt64 0)))
              ,(B.word8X64PrimTyCon, MkVectorTypeDefs 64 B.word8X64PrimTyCon B.word8PrimTyCon B.word8RepDataConTy (Just shuffleWord8X64Prim) broadcastWord8X64Prim packWord8X64Prim unpackWord8X64Prim (GHC.Lit (mkLitWord8 0)))
              ,(B.word16X32PrimTyCon, MkVectorTypeDefs 32 B.word16X32PrimTyCon B.word16PrimTyCon B.word16RepDataConTy (Just shuffleWord16X32Prim) broadcastWord16X32Prim packWord16X32Prim unpackWord16X32Prim (GHC.Lit (mkLitWord16 0)))
              ,(B.word32X16PrimTyCon, MkVectorTypeDefs 16 B.word32X16PrimTyCon B.word32PrimTyCon B.word32RepDataConTy (Just shuffleWord32X16Prim) broadcastWord32X16Prim packWord32X16Prim unpackWord32X16Prim (GHC.Lit (mkLitWord32 0)))
              ,(B.word64X8PrimTyCon, MkVectorTypeDefs 8 B.word64X8PrimTyCon B.word64PrimTyCon B.word64RepDataConTy (Just shuffleWord64X8Prim) broadcastWord64X8Prim packWord64X8Prim unpackWord64X8Prim (GHC.Lit (mkLitWord64 0)))
              ]
#else
  let table = [(B.floatX4PrimTyCon, MkVectorTypeDefs 4 B.floatX4PrimTyCon B.floatPrimTyCon B.floatRepDataConTy Nothing broadcastFloatX4Prim packFloatX4Prim unpackFloatX4Prim (GHC.Lit (mkLitFloat 0)))
              ,(B.doubleX2PrimTyCon, MkVectorTypeDefs 2 B.doubleX2PrimTyCon B.doublePrimTyCon B.doubleRepDataConTy Nothing broadcastDoubleX2Prim packDoubleX2Prim unpackDoubleX2Prim (GHC.Lit (mkLitDouble 0)))
              ,(B.int8X16PrimTyCon, MkVectorTypeDefs 16 B.int8X16PrimTyCon B.int8PrimTyCon B.int8RepDataConTy Nothing broadcastInt8X16Prim packInt8X16Prim unpackInt8X16Prim (GHC.Lit (mkLitInt8 0)))
              ,(B.int16X8PrimTyCon, MkVectorTypeDefs 8 B.int16X8PrimTyCon B.int16PrimTyCon B.int16RepDataConTy Nothing broadcastInt16X8Prim packInt16X8Prim unpackInt16X8Prim (GHC.Lit (mkLitInt16 0)))
              ,(B.int32X4PrimTyCon, MkVectorTypeDefs 4 B.int32X4PrimTyCon B.int32PrimTyCon B.int32RepDataConTy Nothing broadcastInt32X4Prim packInt32X4Prim unpackInt32X4Prim (GHC.Lit (mkLitInt32 0)))
              ,(B.int64X2PrimTyCon, MkVectorTypeDefs 2 B.int64X2PrimTyCon B.int64PrimTyCon B.int64RepDataConTy Nothing broadcastInt64X2Prim packInt64X2Prim unpackInt64X2Prim (GHC.Lit (mkLitInt64 0)))
              ,(B.word8X16PrimTyCon, MkVectorTypeDefs 16 B.word8X16PrimTyCon B.word8PrimTyCon B.word8RepDataConTy Nothing broadcastWord8X16Prim packWord8X16Prim unpackWord8X16Prim (GHC.Lit (mkLitWord8 0)))
              ,(B.word16X8PrimTyCon, MkVectorTypeDefs 8 B.word16X8PrimTyCon B.word16PrimTyCon B.word16RepDataConTy Nothing broadcastWord16X8Prim packWord16X8Prim unpackWord16X8Prim (GHC.Lit (mkLitWord16 0)))
              ,(B.word32X4PrimTyCon, MkVectorTypeDefs 4 B.word32X4PrimTyCon B.word32PrimTyCon B.word32RepDataConTy Nothing broadcastWord32X4Prim packWord32X4Prim unpackWord32X4Prim (GHC.Lit (mkLitWord32 0)))
              ,(B.word64X2PrimTyCon, MkVectorTypeDefs 2 B.word64X2PrimTyCon B.word64PrimTyCon B.word64RepDataConTy Nothing broadcastWord64X2Prim packWord64X2Prim unpackWord64X2Prim (GHC.Lit (mkLitWord64 0)))

              ,(B.floatX8PrimTyCon, MkVectorTypeDefs 8 B.floatX8PrimTyCon B.floatPrimTyCon B.floatRepDataConTy Nothing broadcastFloatX8Prim packFloatX8Prim unpackFloatX8Prim (GHC.Lit (mkLitFloat 0)))
              ,(B.doubleX4PrimTyCon, MkVectorTypeDefs 4 B.doubleX4PrimTyCon B.doublePrimTyCon B.doubleRepDataConTy Nothing broadcastDoubleX4Prim packDoubleX4Prim unpackDoubleX4Prim (GHC.Lit (mkLitDouble 0)))
              ,(B.int8X32PrimTyCon, MkVectorTypeDefs 32 B.int8X32PrimTyCon B.int8PrimTyCon B.int8RepDataConTy Nothing broadcastInt8X32Prim packInt8X32Prim unpackInt8X32Prim (GHC.Lit (mkLitInt8 0)))
              ,(B.int16X16PrimTyCon, MkVectorTypeDefs 16 B.int16X16PrimTyCon B.int16PrimTyCon B.int16RepDataConTy Nothing broadcastInt16X16Prim packInt16X16Prim unpackInt16X16Prim (GHC.Lit (mkLitInt16 0)))
              ,(B.int32X8PrimTyCon, MkVectorTypeDefs 8 B.int32X8PrimTyCon B.int32PrimTyCon B.int32RepDataConTy Nothing broadcastInt32X8Prim packInt32X8Prim unpackInt32X8Prim (GHC.Lit (mkLitInt32 0)))
              ,(B.int64X4PrimTyCon, MkVectorTypeDefs 4 B.int64X4PrimTyCon B.int64PrimTyCon B.int64RepDataConTy Nothing broadcastInt64X4Prim packInt64X4Prim unpackInt64X4Prim (GHC.Lit (mkLitInt64 0)))
              ,(B.word8X32PrimTyCon, MkVectorTypeDefs 32 B.word8X32PrimTyCon B.word8PrimTyCon B.word8RepDataConTy Nothing broadcastWord8X32Prim packWord8X32Prim unpackWord8X32Prim (GHC.Lit (mkLitWord8 0)))
              ,(B.word16X16PrimTyCon, MkVectorTypeDefs 16 B.word16X16PrimTyCon B.word16PrimTyCon B.word16RepDataConTy Nothing broadcastWord16X16Prim packWord16X16Prim unpackWord16X16Prim (GHC.Lit (mkLitWord16 0)))
              ,(B.word32X8PrimTyCon, MkVectorTypeDefs 8 B.word32X8PrimTyCon B.word32PrimTyCon B.word32RepDataConTy Nothing broadcastWord32X8Prim packWord32X8Prim unpackWord32X8Prim (GHC.Lit (mkLitWord32 0)))
              ,(B.word64X4PrimTyCon, MkVectorTypeDefs 4 B.word64X4PrimTyCon B.word64PrimTyCon B.word64RepDataConTy Nothing broadcastWord64X4Prim packWord64X4Prim unpackWord64X4Prim (GHC.Lit (mkLitWord64 0)))

              ,(B.floatX16PrimTyCon, MkVectorTypeDefs 16 B.floatX16PrimTyCon B.floatPrimTyCon B.floatRepDataConTy Nothing broadcastFloatX16Prim packFloatX16Prim unpackFloatX16Prim (GHC.Lit (mkLitFloat 0)))
              ,(B.doubleX8PrimTyCon, MkVectorTypeDefs 8 B.doubleX8PrimTyCon B.doublePrimTyCon B.doubleRepDataConTy Nothing broadcastDoubleX8Prim packDoubleX8Prim unpackDoubleX8Prim (GHC.Lit (mkLitDouble 0)))
              ,(B.int8X64PrimTyCon, MkVectorTypeDefs 64 B.int8X64PrimTyCon B.int8PrimTyCon B.int8RepDataConTy Nothing broadcastInt8X64Prim packInt8X64Prim unpackInt8X64Prim (GHC.Lit (mkLitInt8 0)))
              ,(B.int16X32PrimTyCon, MkVectorTypeDefs 32 B.int16X32PrimTyCon B.int16PrimTyCon B.int16RepDataConTy Nothing broadcastInt16X32Prim packInt16X32Prim unpackInt16X32Prim (GHC.Lit (mkLitInt16 0)))
              ,(B.int32X16PrimTyCon, MkVectorTypeDefs 16 B.int32X16PrimTyCon B.int32PrimTyCon B.int32RepDataConTy Nothing broadcastInt32X16Prim packInt32X16Prim unpackInt32X16Prim (GHC.Lit (mkLitInt32 0)))
              ,(B.int64X8PrimTyCon, MkVectorTypeDefs 8 B.int64X8PrimTyCon B.int64PrimTyCon B.int64RepDataConTy Nothing broadcastInt64X8Prim packInt64X8Prim unpackInt64X8Prim (GHC.Lit (mkLitInt64 0)))
              ,(B.word8X64PrimTyCon, MkVectorTypeDefs 64 B.word8X64PrimTyCon B.word8PrimTyCon B.word8RepDataConTy Nothing broadcastWord8X64Prim packWord8X64Prim unpackWord8X64Prim (GHC.Lit (mkLitWord8 0)))
              ,(B.word16X32PrimTyCon, MkVectorTypeDefs 32 B.word16X32PrimTyCon B.word16PrimTyCon B.word16RepDataConTy Nothing broadcastWord16X32Prim packWord16X32Prim unpackWord16X32Prim (GHC.Lit (mkLitWord16 0)))
              ,(B.word32X16PrimTyCon, MkVectorTypeDefs 16 B.word32X16PrimTyCon B.word32PrimTyCon B.word32RepDataConTy Nothing broadcastWord32X16Prim packWord32X16Prim unpackWord32X16Prim (GHC.Lit (mkLitWord32 0)))
              ,(B.word64X8PrimTyCon, MkVectorTypeDefs 8 B.word64X8PrimTyCon B.word64PrimTyCon B.word64RepDataConTy Nothing broadcastWord64X8Prim packWord64X8Prim unpackWord64X8Prim (GHC.Lit (mkLitWord64 0)))
              ]
#endif
  pure $ MkPluginDefs {..}

freshId :: String -> Type -> TcPluginM Solve Id
freshId base ty = do
  uniq <- unsafeLiftTcM GHC.getUniqueM
  let name = GHC.mkSystemName uniq (mkVarOcc base)
  pure $ GHC.mkLocalId name GHC.manyDataConTy ty

{-
extractTypeMaybe :: PluginDefs -> Type -> Maybe (Maybe Type)
extractTypeMaybe MkPluginDefs{..} t = case splitTyConApp_maybe t of
  Just (tc, [_k]) | tc == promotedNothingDataCon -> Just Nothing
  Just (tc, [_k, x]) | tc == promotedJustDataCon -> Just (Just x)
  _ -> Nothing

extractTypeEither :: PluginDefs -> Type -> Maybe (Either Type Type)
extractTypeEither MkPluginDefs{..} t = case splitTyConApp_maybe t of
  Just (tc, [_k0, _k1, x]) | tc == promotedLeftDataCon -> Just (Left x)
                           | tc == promotedRightDataCon -> Just (Right x)
  _ -> Nothing
-}

extractTypeList :: Type -> Maybe [Type]
extractTypeList = go []
  where
    go revAcc t = case splitTyConApp_maybe t of
      Just (tc, [_k, x, xs]) | tc == B.promotedConsDataCon -> go (x : revAcc) xs
      Just (tc, [_k]) | tc == B.promotedNilDataCon -> Just (reverse revAcc)
      _ -> Nothing

extractIndices :: Type -> Maybe [Integer]
extractIndices t = do
  xs <- extractTypeList t
  forM xs $ \v -> do
    i <- isNumLitTy v
    guard (0 <= i)
    pure i

fillIndices :: DynFlags -> Int -> [Maybe Integer] -> [Integer]
fillIndices _dynflags n mi =
  let patterns = tryPattern [0..nI-1] -- no-op
                 <|> tryPattern [nI..2*nI-1] -- no-op
                 <|> tryPattern (interleave [0..] [nI..]) -- UNPCKL
                 <|> tryPattern (interleave [nI..] [0..]) -- UNPCKL
                 <|> tryPattern (interleave [nI `quot` 2..] [nI + nI `quot` 2..]) -- UNPCKH
                 <|> tryPattern (interleave [nI + nI `quot` 2..] [nI `quot` 2..]) -- UNPCKH
                 <|> blend -- BLEND
  in case patterns of
       Just x  -> x
       Nothing -> map (fromMaybe 0) mi
  where
    nI = toInteger n
    -- platform = targetPlatform dynflags
    compatible pat = and [p == i | (p, Just i) <- zip pat mi]
    tryPattern pat = guard (compatible pat) >> Just (take n pat)
    blend = guard (and [p `rem` nI == i `rem` nI | (p, Just i) <- zip [0..] mi]) >> Just (zipWith fromMaybe [0..] mi)
    interleave (x : xs) (y : ys) = x : y : interleave xs ys
    interleave _ _               = []

buildShuffle :: DynFlags -> VectorTypeDefs -> Id -> CoreExpr -> [(Integer, (Integer, Integer))] -> CoreExpr
buildShuffle dynflags defs@(MkVectorTypeDefs {..}) shuffleId' vExp pairs =
  let n = numberOfElements
      nI = toInteger n
      platform = targetPlatform dynflags
      makeZeroVector = GHC.App (GHC.Var broadcastId) zeroExpr
      -- makeZeroVector' = GHC.App (GHC.Var packId) $ GHC.mkCoreConApps (GHC.tupleDataCon GHC.Unboxed n) $ replicate n (GHC.Type scalarRepDataConTy) ++ replicate n (GHC.Type $ mkTyConTy scalarTyCon) ++ replicate n zeroExpr
      makeBinaryShuffle xExp yExp mindices =
        let numIndices = fillIndices dynflags n mindices
            constIndices = GHC.mkCoreConApps (GHC.tupleDataCon GHC.Unboxed n) $ replicate n (GHC.Type GHC.intRepDataConTy) ++ replicate n (GHC.Type B.intPrimTy) ++ map (GHC.Lit . GHC.mkLitInt platform) numIndices
        in mkCoreApps (GHC.Var shuffleId') [xExp, yExp, constIndices]
  in case nub [srcVec | (_, (srcVec, _)) <- pairs] of
    [] -> makeZeroVector
    [s0] -> makeBinaryShuffle (GHC.App vExp $ mkNaturalExpr platform s0) makeZeroVector [(\(_, srcLane) -> srcLane) <$> lookup i pairs | i <- [0..nI-1]]
    [s0, s1] -> makeBinaryShuffle (GHC.App vExp $ mkNaturalExpr platform s0) (GHC.App vExp $ mkNaturalExpr platform s1) [(\(srcVec, srcLane) -> if srcVec == s0 then srcLane else srcLane + nI) <$> lookup i pairs | i <- [0..nI-1]]
    s0 : _ -> makeBinaryShuffle (GHC.App vExp $ mkNaturalExpr platform s0) (buildShuffle dynflags defs shuffleId' vExp (filter (\(_, (srcVec, _)) -> srcVec /= s0) pairs)) [(\(srcVec, srcLane) -> if srcVec == s0 then srcLane else i + nI) <$> lookup i pairs | i <- [0..nI-1]]

buildShuffleWithPack :: DynFlags -> VectorTypeDefs -> CoreExpr -> [(Integer, (Integer, Integer))] -> TcPluginM Solve CoreExpr
buildShuffleWithPack dynflags (MkVectorTypeDefs {..}) vExp pairs =
  let n = numberOfElements
      platform = targetPlatform dynflags
      go [] _ vars =
        let defaultExpr = case vars of
              (_, e:_):_ -> GHC.Var e
              _          -> zeroExpr
            args = [ case lookup dst pairs of
                       Nothing -> defaultExpr
                       Just (srcVec, srcLane) -> GHC.Var (fromMaybe (panic "no srcVec") (lookup srcVec vars) !! fromInteger srcLane)
                   | dst <- [0..toInteger n-1]
                   ]
        in pure $ GHC.App (GHC.Var packId) (GHC.mkCoreConApps (GHC.tupleDataCon GHC.Unboxed n) $ replicate n (GHC.Type scalarRepDataConTy) ++ replicate n (GHC.Type $ mkTyConTy scalarTyCon) ++ args)
      go (srcVec : vecs) k vars = do
        v <- freshId "v" $ GHC.mkTyConApp (B.tupleTyCon GHC.Unboxed n) $ replicate n scalarRepDataConTy ++ replicate n (mkTyConTy scalarTyCon)
        elems <- replicateM n (freshId "x" $ mkTyConTy scalarTyCon)
        rest <- go vecs (k + n) ((srcVec, elems) : vars)
        let vecTy = GHC.mkTyConApp tyCon []
        pure $ GHC.Case (GHC.App (GHC.Var unpackId) (GHC.App vExp $ mkNaturalExpr platform srcVec)) v vecTy [GHC.Alt (GHC.DataAlt (GHC.tupleDataCon GHC.Unboxed n)) elems rest]
  in go (nub [srcVec | (_, (srcVec, _)) <- pairs]) 0 []

pluginSolve :: PluginDefs -> [Ct] -> [Ct] -> TcPluginM Solve TcPluginSolveResult
pluginSolve (MkPluginDefs {..}) _givens wanteds = do
  -- tcPluginTrace "---Plugin start---" (ppr givens $$ ppr wanteds)
  dynflags <- unsafeLiftTcM getDynFlags
  (insoluble, solved) <- fmap (partitionEithers . concat) . forM wanteds $ \ct ->
    case classifyPredType $ ctPred ct of
      ClassPred cls typeArgs@[_rep, vecTy, typeIndices]
        | cls == shuffleManyClass
        , Just (vcon, []) <- splitTyConApp_maybe vecTy
        , Just indices <- extractIndices typeIndices
        , Just vectorTypeDefs@(MkVectorTypeDefs { shuffleId }) <- lookup vcon table -> do
          let n = numberOfElements vectorTypeDefs
              effectiveSize = length indices
          if effectiveSize <= n
            then do
              vVar <- freshId "v" (mkVisFunTyMany B.naturalTy vecTy)
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0)
              let hasShufflePrim = True
#else
              let hasShufflePrim = case backendCodeOutput (backend dynflags) of
                    NcgCodeOutput  -> isAvxEnabled dynflags
                    LlvmCodeOutput -> True
                    _              -> False
#endif
              body <- case shuffleId of
                Just shuffleId' | hasShufflePrim -> pure $ buildShuffle dynflags vectorTypeDefs shuffleId' (GHC.Var vVar) (zip [0..] $ map (`quotRem` toInteger effectiveSize) indices)
                _ -> buildShuffleWithPack dynflags vectorTypeDefs (GHC.Var vVar) (zip [0..] $ map (`quotRem` toInteger effectiveSize) indices)
              pure [Right (EvExpr (evUnaryDictAppE cls typeArgs $ GHC.Lam vVar body), ct)]
            else pure [Left ct]
      ClassPred cls typeArgs@[ty, typeIndex]
        | cls == pickClass
        , Just i <- isNumLitTy typeIndex
        , 0 <= i -> do
          vVar <- freshId "v" (mkVisFunTyMany B.naturalTy ty)
          let platform = targetPlatform dynflags
              body = GHC.App (GHC.Var vVar) $ mkNaturalExpr platform i
          pure [Right (EvExpr (evUnaryDictAppE cls typeArgs $ GHC.Lam vVar body), ct)]
      _ -> pure []
  if null insoluble
    then pure $ TcPluginOk solved []
    else pure $ TcPluginContradiction insoluble
