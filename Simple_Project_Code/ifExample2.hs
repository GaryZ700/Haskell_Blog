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
               then do
                       putStrLn "Wake UUUUUUUUUUUPPP! :)"
                       putStrLn "Pleaase wake up or they are going to fire me :( "
                       putStrLn "Pretty please wake up, you don't want to see me cry do you?"
                       putStrLn "Wahahaaaaa!"
           else putStrLn ("I don't now what " ++ animal ++ " is, but that's cool I guess.")

           putStrLn "I hope you found this amusing."
