main :: IO ()
main = do
  putStr $ unlines $ do
    (n,f) <- [("Sum","mkSum")
             ,("getSum","getSum'")
             ,("Product","mkProduct")
             ,("getProduct","getProduct'")
             ,("Min","mkMin")
             ,("getMin","getMin'")
             ,("Max","mkMax")
             ,("getMax","getMax'")
             ]
    x <- ["X2","X4","X8","X16","X32"]
    ["\"liftSIMD/" ++ n ++ "/" ++ x ++ "\"","  liftSIMD coerce = " ++ f ++ " @" ++ x]
  putStr $ unlines $ do
    (n,lhs,rhs) <- [("Complex","(:+)","mkComplex")
                   ,("(,)","(,)","mkTuple2")
                   ]
    x <- ["X2","X4","X8","X16","X32"]
    ["\"liftSIMD2/" ++ n ++ "/" ++ x ++ "\"","  liftSIMD2 " ++ lhs ++ " = " ++ rhs ++ " @" ++ x]
