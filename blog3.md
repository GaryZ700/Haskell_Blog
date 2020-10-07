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
           putStrLn ( (generateSpace 20 ' ') ++ "." )
           main
      
generateSpace :: Int -> Char -> String
generateSpace 0 c = []
generateSpace i c = c : generateSpace (i-1) c</code></pre></code></pre>
Well, it seemed that works after running it. But now we have a new problem, the "balls" only appear after twenty spaces have passed, meaning that we need that value to dynamically shift per cycle. Since I do not think there is a way to have a variable value persist between recursive calls without an input parameter, we may need a new function to handle this. 
<pre><code>main :: IO ()
main = do 
           simulateBall 50
           main 
 
 simulateBall :: Int -> IO ()
 simulateBall 0 = return ()
 simulateBall i = do 
                      putStrLn ( (generateSpaces i ' ') ++ ".")
                      simulateBall (i-1)
 
generateSpace :: Int -> Char -> String
generateSpace 0 c = []
generateSpace i c = c : generateSpace (i-1) c</code></pre></code></pre>
Ok, awesome! Now we're really cooking, but it seems this code only simulates the ball's forward movement, not its bounceback to the original "wall". 
