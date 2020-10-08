# CPSC 354: Haskell Blog 

## 09-06-2020 A Simple Project 
If you have been following along in this blog so far, then congratulations!! We have learned so much Haskell that now its time to start applying our knowledge and see what we can build. For this post, I'll be describing how to build a stream of dots that bounces off a boundary. Although we may feel like tamers of the wild and elusive Haskell language, there is still a lot that there is to be learned about it and there will be more blogs focused on learning the concepts along with more hands on and practical posts. 

## The Project
The point of this project will be to develop a program that will simulate a ball hitting two invisible walls on the terminal, and bouncing back and forth. Instead of a ball, a period will be used instead. The desired output will look something like this: 
<pre></code>.
        .
       .
      .
     .
    .
   .
  .
 .
  .
    .
     .
      .
       .
        .
         .
.</code></pre>
This pattern will then continue to repeat itself until the user decides to kill the program via ctrl-c. 

## Implementation
Where do we start? Well, since we will printing data out onto the screen we certainly will be needing to use IO in Haskell, implying we will need a main function. Open up a new Haskell file and name it ball.hs. Start off the file by declaring the main function: 
<pre><code>main :: IO ()
main = do 
           putStrLn "."</code></pre>
Ok, let's run this using <code>runghc ball.hs</code> and see what happens. Unsurprisingly, executing this program results in a single "ball" being printed to the screen. How do we make it repeat itself? How about if we use recursion on the main expression? Let's give it a go. 
<pre><code>main = do 
           putStrLn "."
           main</code></pre></code></pre>
Once again, let's run this and see what happens. It looks like we got stuck within an infinite loop of periods being printed to the terminal. Oh, boy how are we going to get out of this? Hit ctrl-c or cmd-c if you are on a Windows or a Mac respectively, and that will kill the Haskell application. Now that we're out of that, we need to find a way to move the "ball" forward once per cycle of the code to simulate a moving ball. Let's make a function that takes an integer and a char, and then returns out that amount of chars in a string, then we can concatenate the period to the output of this function?
<pre><code>main = do 
           putStrLn ( (generateChar 20 ' ') ++ "." )
           main
      
generateChar :: Int -> Char -> String
generateChar 0 c = []
generateChar i c = c : generateChar (i-1) c</code></pre></code></pre>
Well, it seemed that works after running it. But now we have a new problem, the "balls" only appear after twenty spaces have passed, meaning that we need that value to dynamically shift per cycle. Since I do not think there is a way to have a variable value persist between recursive calls without an input parameter, we may need a new function to handle this. 
<pre><code>main :: IO ()
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
generateChar i c = c : generateChar (i-1) c</code></pre></code></pre>
Ok, awesome! Now we're really cooking, but it seems this code only simulates the ball's forward movement, not its bounceback to the original "wall". What it seems we need to do to make the simulate function more generic is to have it accept a start and end position for the simulation, where the start and end represent the "walls" that the ball bounces off of each time it reaches that position. Thus, our function would look more like this: <code>simulateBall :: Int -> Int -> IO ()</code>, which supports two interger inputs, one for the start and another for the end value. But, then how would we differentiate between when the ball start at position 10 and ends at position 0, versus starting at position 0 and ending at 10? It seems we need a new control structure, an if statement to determine if the ball will be traveling from left to right, or from right to left. 

#if Statements
The following discussion is based upon the information presented <code href="https://en.wikibooks.org/wiki/Haskell/Control_structures">here.</code>. An if statement in Haskell follows an if then, else if then, else structure, which can be seen in the example below: 
<pre><code>main :: IO ()
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
           else putStrLn "I don't now what " ++ animal ++ " is, but that's cool I guess."
            
           putStrLn "I hope you found this amusing." </code></pre>
This is a very contrived example, and does not account for different cases that a word can be entered in but still ilustrates the basics of if structures. As you can see, each if is followed by a then statement, further followed by the code statement to execute. The overall logic functions as one would expect, if the user inputs an animal that the if statement is checking for, then the if executes the associated code, and the final print separated from the if block will always execute. There is one limitation to Haskell ifs, each if can only execute one block of code after it. 

There is a way to circumnavigate this limitatation by using do blocks for the if statement that needs to execute multiple code lines: 
<pre><code>main :: IO ()
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
</code></pre>
    
