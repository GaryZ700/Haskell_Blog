main :: IO ()
main = do
           simulateBall 10 20
           simulateBall 20 10
           main

simulateBall :: Int -> Int -> IO ()
simulateBall s e = do
                     getLine
                     if s > e
                         then do
                                 putStrLn ( (generateChar s ' ')  ++ ".")
                                 simulateBall (s-1) e
                     else if e > s
                            then do
                                    putStrLn ( (generateChar s ' ')  ++ ".")
                                    simulateBall (s+1) e
                     else return ()

generateChar :: Int -> Char -> String
generateChar 0 c = []
generateChar i c = c : generateChar (i-1) c
