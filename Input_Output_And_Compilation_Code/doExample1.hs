main :: IO ()
main = do
       putStrLn "Please enter your name: "
       name <- getLine
       putStrLn ("Goodbye " ++ name ++ "!")
