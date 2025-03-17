import Data.List (intercalate)
import Control.Monad
import System.Environment

types :: [(String, Int)]
types = [("Float", 4)
        ,("Double", 8)
        ,("Int8", 1)
        ,("Int16", 2)
        ,("Int32", 4)
        ,("Int64", 8)
        ,("Word8", 1)
        ,("Word16", 2)
        ,("Word32", 4)
        ,("Word64", 8)
        ]

main :: IO ()
main = do
  args <- getArgs
  let w = case args of
            "128":_ -> 16
            "256":_ -> 32
            "512":_ -> 64
            _ -> 16
  putStr $ unlines $ concat
    [ [ "broadcast" ++ t ++ "X" ++ show i ++ "# :: " ++ t ++ "# -> " ++ t ++ "X" ++ show i ++ "#"
      , "broadcast" ++ t ++ "X" ++ show i ++ "# x = M.pack" ++ t ++ "X" ++ show i ++ "# (# " ++ intercalate ", " (replicate i "x") ++ " #)"
      , "{-# INLINE broadcast" ++ t ++ "X" ++ show i ++ "# #-}"
      , ""
      ]
    | (t, e) <- types
    , let i = w `quot` e
    ]
