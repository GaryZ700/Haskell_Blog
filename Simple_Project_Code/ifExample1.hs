main :: IO ()
main = do
           putStrLn "What is your favorite animal?"
           animal <- getLine
           if animal == "cat"
               then putStrLn "Meow!"
           else if animal == "cow"
               then putStrLn "Moo!"
           else if animal == "Monkey"
               then putStrLn "I want a banana!"
           else if animal == "rooster"
               then putStrLn "Wake UUUUUUUUUUUPPP! :)"
           else putStrLn ("I don't now what " ++ animal ++ " is, but that's cool I guess.")

           putStrLn "I hope you found this amusing."
