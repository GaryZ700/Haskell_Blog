main = do
           putStrLn ( (generateChar 20 ' ') ++ "." )
           main

generateChar :: Int -> Char -> String
generateChar 0 c = []
generateChar i c = c : generateChar (i-1) c
