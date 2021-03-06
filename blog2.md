# CPSC 354: Haskell Blog  

## 10-02-2020: Input, Output & Compilation

For today's exciting adventure in Haskell, we will be discussing input and output, as well as leveraging <code>ghc</code> to compile Haskell code into an executable file. The information presented in this blog originated from the <a href="http://learnyouahaskell.com/input-and-output">following site</a>. 

## Haskell IO

To begin with, the idea of IO in Haskell goes against the fundamental design of the language. As mentioned before, Haskell is a functional programming language, meaning that it executes commands via evaluation of expressions. This limits Haskell from performing what is known as state changes, meaning that Haskell expressions can only return values but can not affect any other aspect of the system or memory. To better illustrate this point, let's consider Python. In Python, a function can modify a list whose memory location can be modifed in another part of the program, allowing the function to create an effect that lasts outside of its scope. To be a bit more concrete, if the list <code>[1,2]</code> were passed into the function <code>listModifier</code>:
<pre><code>def listModifier(a):
    a.append(0)
</code></pre>
Then, if the list were to be accessed outside of the function <code>listModifier</code>, it would still contain the value 0. Thus, the function did not change the local value of the variable <code>a</code>, but instead modified its memory location so that the effect would last outside of the function. If Python were a functional programming lanaguage, then variable modifications that lasted outside of a function would be prohibited, and the only way for the function to comunicate to the larger global scope would be to return a value, the equivalent of evaluating an expression in Haskell. 

The ideological and mathematical purity of Haskell creates a challenge to IO in that IO requires an expression to actively write to the screen or pull input from the user, breaking the functional programming paradigm. IO does so by allowing expressions to be evaluated to some value, but also allows the evaluation itself to affect the state of the program by performing IO communication. This introduces the idea of the <code>main</code> expression, which is of the type <code>IO ()</code>. The <code>IO</code> represents the main function's ability to perform IO calls, while <code>()</code> is the return type of the <code>main</code> expression, meaning that it returns nothing. The <code>main</code> expression also acts similar to the main function in C/C++ in that when compiled to produce an executable, <code>main</code> is the starting entry point of code execution. 

To start off, let's become familiar with the output and input function in Haskell. Go to a command line and enter <code>ghci</code> to launch the interactive Haskell interpreter. Within the interpreter, enter <code>putStrLn</code> followed by a string and press enter. You should see the string printed on the screen. To get input, type the <code>getLine</code> command into the interpreter, press enter and then enter another string. This second string should also be displayed onto the screen.
  
An interesting experiment is to try the following lines of code in the <code>ghci</code>:
<pre><code>x = getLine
x</code></pre>
After entering the above two lines of code, did you receive the output you were expecting? I certainly didn't the first time I tried it! Simply put, <code>x = getLine</code> means that the IO expression named <code>getLine</code> is being renamed to a new expression called <code>x</code>, instead of assigning the output of <code>getLine</code> to <code>x</code>. This is due to the functional nature of Haskell, but there is a way for the value of <code>gerLine</code> to be stored properly into a variable of our choosing in string form. Try the following two lines of code in your terminal and see what happens: 
<pre><code>x <- getLine
x</code></pre>
The above example will demonstrate that <code>x</code> is storing the output from <code>getLine</code>.

## Compilation 
Now that the basics of Haskell IO have been introduced, we can move onto compiling our first Haskell program. First open a new Haskell file, (remember, Haskell files have an extension of .hs), and enter in the following code: 
<pre><code>main :: IO ()
main = putStrLn "Hello Haskell World!"</code></pre>
To compile this file, enter <code>ghc</code> followed by the location and name of the file to compile it. Once <code>ghc</code> has completed its job, an executable file of the same name as the Haskell file will be created in the code file directory. Run the executable as you would run any application from the command line. 

You should see the phrase "Hello Haskell World" show up on screen. Another method to compile this program is to use the <code>runghc</code> command followed by the name of the file. Unlike <code>ghc</code>, <code>runghc</code> does not produce an executable file but instead immediately compiles and runs the code in place. 

The next step would be to add input to our simple program, but there is a slight problem with doing so. Recall that in Haskell, functions can only evaluate expressions, there is no way for the main expression to call both the <code>putStrLn</code> and the <code>getLine</code> code together. 

## do Structure

The <code>do</code> structure is fascinating in Haskell because it directly allows for an almost imperative exeuction of step by step commands. In other words, using <code>do</code>, it is possible for the main expression to execute both input and output operations. The following code snippet ilustrates this, and it is highly recomended that you copy or type this code into your own Haskell file to try for yourself. 
<pre><code>main :: IO ()
main = do
       putStrLn "Please enter your name: "
       name <- getLine
       putStrLn ("Goodbye " ++ name ++ "!")
</code></pre>
Just like that, we get Python like code execuation within Haskell. The <code>do</code> structure also allows for this style: 
<pre><code>main :: IO ()
main = do putStrLn "Please enter your name: "; name <- getLine; putStrLn ("Goodbye " ++ name ++ "!")</code></pre>

## Subfunctions with main 

The main expression can also call other variables and expressions within the program due to the <code>do</code> structure. An example of this follows below: 
<pre><code>main :: IO ()
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
            shout "segmentation fault (core dump)"</code></pre>

## References
<ul>
    <li><a href="http://learnyouahaskell.com/input-and-output">Learn You Haskell</a></li>
</ul>

##  Source Code
<a href="https://github.com/GaryZ700/Haskell_Blog/tree/master/Input_Output_And_Compilation_Code">Click here</a> to access the source code used in this post. 

<b><a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog3.md">Next Post</a></b><br/>
<b><a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog1.md">Previous Post</a></b>
