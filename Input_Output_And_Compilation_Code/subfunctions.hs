main :: IO ()
main = do
       name <- getName
       shout ("I like cats " ++ name)
       shoo name

getName :: IO String
getName = do
          putStrLn "Please enter your name: "
          getLine

shout :: String -> IO ()
shout phrase = putStrLn (phrase ++ "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! (I'm shouting at if you can't tell >:(  )")

shoo :: String -> IO ()
shoo name = do
            putStrLn ("Shoo " ++ name)
            putStrLn "What did I say!?!!? Shoo go away!!"
            shout "That's it I'm done"
            shout "segmentation fault (core dump)"
