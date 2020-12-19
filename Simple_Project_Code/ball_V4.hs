main :: IO ()
main = do
           simulateBall 50
           main

simulateBall :: Int -> IO ()
simulateBall 0 = return ()
simulateBall i = do
                      putStrLn ( (generateChar i ' ') ++ ".")
                      simulateBall (i-1)

generateChar :: Int -> Char -> String
generateChar 0 c = []
generateChar i c = c : generateChar (i-1) c
