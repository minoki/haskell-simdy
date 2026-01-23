import System.Environment

data T = INT | WORD | FLOAT deriving Eq

types128, types256, types512 :: String -> [(String, String, T, String)]
types128 p =
  [("Int8X16", "Int8", INT, p)
  ,("Int16X8", "Int16", INT, p)
  ,("Int32X4", "Int32", INT, p)
  ,("Int64X2", "Int64", INT, p)
  ,("Word8X16", "Word8", WORD, p)
  ,("Word16X8", "Word16", WORD, p)
  ,("Word32X4", "Word32", WORD, p)
  ,("Word64X2", "Word64", WORD, p)
  ,("FloatX4", "Float", FLOAT, p)
  ,("DoubleX2", "Double", FLOAT, p)
  ]

types256 p =
  [("Int8X32", "Int8", INT, p)
  ,("Int16X16", "Int16", INT, p)
  ,("Int32X8", "Int32", INT, p)
  ,("Int64X4", "Int64", INT, p)
  ,("Word8X32", "Word8", WORD, p)
  ,("Word16X16", "Word16", WORD, p)
  ,("Word32X8", "Word32", WORD, p)
  ,("Word64X4", "Word64", WORD, p)
  ,("FloatX8", "Float", FLOAT, p)
  ,("DoubleX4", "Double", FLOAT, p)
  ]

types512 p =
  [("Int8X64", "Int8", INT, p)
  ,("Int16X32", "Int16", INT, p)
  ,("Int32X16", "Int32", INT, p)
  ,("Int64X8", "Int64", INT, p)
  ,("Word8X64", "Word8", WORD, p)
  ,("Word16X32", "Word16", WORD, p)
  ,("Word32X16", "Word32", WORD, p)
  ,("Word64X8", "Word64", WORD, p)
  ,("FloatX16", "Float", FLOAT, p)
  ,("DoubleX8", "Double", FLOAT, p)
  ]

main :: IO ()
main = do
  args <- getArgs
  let types = case args of
                "256":_ -> types256 "M."
                "512":_ -> types512 "M."
                _ -> types128 "M."
  putStr $ unlines $ map (\ident -> case ident of '#':_ -> ident; _ -> "  , " ++ ident) $ concat
    [[p ++ t ++ "#" | (t, _, _, p) <- types]
    ,[(if p == "M." then "" else p) ++ "broadcast" ++ t ++ "#" | (t, _, _, p) <- types]
    ,[p ++ "pack" ++ t ++ "#" | (t, _, _, p) <- types]
    ,[p ++ "unpack" ++ t ++ "#" | (t, _, _, p) <- types]
    ,[p ++ "insert" ++ t ++ "#" | (t, _, _, p) <- types]
    ,[p ++ "plus" ++ t ++ "#" | (t, _, _, p) <- types]
    ,[p ++ "minus" ++ t ++ "#" | (t, _, _, p) <- types]
    ,[p ++ "times" ++ t ++ "#" | (t, _, _, p) <- types]
    ,[p ++ "divide" ++ t ++ "#" | (t, _, k, p) <- types, k == FLOAT]
    ,[p ++ "quot" ++ t ++ "#" | (t, _, k, p) <- types, k == INT || k == WORD]
    ,[p ++ "rem" ++ t ++ "#" | (t, _, k, p) <- types, k == INT || k == WORD]
    ,[p ++ "negate" ++ t ++ "#" | (t, _, k, p) <- types, k == INT || k == FLOAT]
    ,[p ++ "index" ++ t ++ "Array#" | (t, _, _, p) <- types]
    ,[p ++ "read" ++ t ++ "Array#" | (t, _, _, p) <- types]
    ,[p ++ "write" ++ t ++ "Array#" | (t, _, _, p) <- types]
    ,[p ++ "index" ++ t ++ "OffAddr#" | (t, _, _, p) <- types]
    ,[p ++ "read" ++ t ++ "OffAddr#" | (t, _, _, p) <- types]
    ,[p ++ "write" ++ t ++ "OffAddr#" | (t, _, _, p) <- types]
    ,[p ++ "index" ++ s ++ "ArrayAs" ++ t ++ "#" | (t, s, _, p) <- types]
    ,[p ++ "read" ++ s ++ "ArrayAs" ++ t ++ "#" | (t, s, _, p) <- types]
    ,[p ++ "write" ++ s ++ "ArrayAs" ++ t ++ "#" | (t, s, _, p) <- types]
    ,[p ++ "index" ++ s ++ "OffAddrAs" ++ t ++ "#" | (t, s, _, p) <- types]
    ,[p ++ "read" ++ s ++ "OffAddrAs" ++ t ++ "#" | (t, s, _, p) <- types]
    ,[p ++ "write" ++ s ++ "OffAddrAs" ++ t ++ "#" | (t, s, _, p) <- types]
    -- GHC 9.12 or later
    ,["#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)"]
    ,[p ++ "fmadd" ++ t ++ "#" | (t, _, k, p) <- types, k == FLOAT]
    ,[p ++ "fmsub" ++ t ++ "#" | (t, _, k, p) <- types, k == FLOAT]
    ,[p ++ "fnmadd" ++ t ++ "#" | (t, _, k, p) <- types, k == FLOAT]
    ,[p ++ "fnmsub" ++ t ++ "#" | (t, _, k, p) <- types, k == FLOAT]
    ,[p ++ "shuffle" ++ t ++ "#" | (t, _, _, p) <- types]
    ,[p ++ "min" ++ t ++ "#" | (t, _, _, p) <- types]
    ,[p ++ "max" ++ t ++ "#" | (t, _, _, p) <- types]
    ,["#endif"]
    ]
