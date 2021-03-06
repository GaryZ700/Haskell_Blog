# CPSC 354: Haskell Blog  

## 11-08-2020:  It's all Fun & Games till Someone gets Stacked, Part 3

Before we start, remember to take a look at <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog4.md">Part 1</a> and <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog5.md">Part 2</a> of this blog post to ensure you have the needed understanding of the project and the Haskell stack. 

If you have the needed background info, then let's get going! For today, we will (finally) begin programmign the Word Game logic in Haskell. To start off with, the majority of the logic for this project will be written within the Lib.hs file located in the src folder. Navigate to Lib.hs and open it within the text editor of you choice and we can get going from there. 

## The Grid 
The main idea behind this game involves the use of a game grid containing the word serach itself. Let's start off by creating a word search 20 characters wide by 10 characters tall. For the sake of this blog, I will be focusing on a theme of physics and science related verbiage. To be exact, for my word search the following words will be included: 
<ul>
  <li>EXPERIMENT</li>
  <li>ENTROPY</li>
  <li>PHYSICS</li>
  <li>CHEMISTRY</li>
  <li>LAB</li>
  <li>MATH</li>
  <li>SPACE</li>
  <li>MOLECULES</li>
  <li>HYPOTHESIS</li>
</ul>
You are more than welcome to follow along using the words and theme that I have choosen for this example, or you can come up with your own theme for some extra fun. Now, for the sake of convenience, we as the programmer will be providing the computer with the layout and organization of the words. But, in the future this program could be expanded to randomly place words into the grid. 
<pre><code>LAB****************E
CHEMISTRY*********X*
********M********P*P
*********A******E**H
SELUCELOM*T****R***Y
***********H**I****S
***ECAPS*****M*****I
HYPOTHESIS**E******C
***********N*******S
**********T*********
*****ENTROPY********</code></pre>
Spaces taken up by a * represent random letters that we will later be adding in to have a proper word search. First things first is to add this grid to the Lib.hs file by adding in the following code: 
<pre>grid = [ "LAB****************E"
            , "CHEMISTRY*********X*"
            , "********M********P*P"
            , "*********A******E**H"
            , "SELUCELOM*T****R***Y"
            , "***********H**I****S"
            , "***ECAPS*****M*****I"
            , "HYPOTHESIS**E******C"
            , "***********N*******S"
            , "**********T*********"
            , "*****ENTROPY********"
            ]
</code></pre>
As can be seen, the grid will be represented by a list of strings, each string representing one row of the word search. Note, that although the comma can be placed at the end of the line for each string, Haskell convention dictates that the comma be placed on the left side and aligned with the brackets for the list. Next, add the variable name grid to the top of the Haskell file in the parenthesis for <code>module lib</code> so that the snipit looks like this: 
<pre><code>module Lib 
    ( someFunc, 
       grid
     ) where</code></pre>
Adding grid to the this area of the code allows the variable to be publically be accessed outside of this module, specifically in the interactive interpretor to allow us to easily test the code. Now that we have a publically accessible game grid, the question of how this grid will be displayed nicely to the user arises. 

## Displaying the Grid
To see how we shall test our grid code, enter <code>stack ghci</code> into a terminal located inside the WordGame folder. This command will compile our code so far and place it within the GHCI for us to interact with. Try typing <code>grid</code> into the GHCI and see what happens. As you most likely see, instead of printing out a lovely rectangle of words and asterisks, we were instead greated by a linearly displayed list of strings. In order to properly display the graph onto the screen we will need to utilize the <code>unlines</code> function. 

## Unlines Function
Simply put, the <code>unlines</code> function takes a list of strings and returns a single string. The concatentation of the rows of the list involves also adding a newline character between rows, making it create a very nicely formatted string for display to the user. Below follows an example of how the <code>unlines</code> function works: 
<pre><code>test = ["cat", "dog", "mouse"]
unlines test </code></pre>
Produces the following result: <code>"cat\ndog\nmouse\n"</code>

Combining the <code>ulines</code> function with the <code>putStrLn</code> function will allow us to easily display the grid to the user. Test this in the stack GHCI by typing in <code>putStrLn (unlines grid)</code>.
Gee wiz....ain't that a beauty! The <code>putStrLn</code> command is needed in order to display the output of the <code>unlines</code> properly onscreen as opposed to literally displaying a string of characters separated by actual <code>\n</code>.

Now, we can utilize this knowledge to add a function to the Lib.hs file that allows us to print out the grid properly to the user: 
<pre><code>displayGrid :: [STRING] -> IO ()
displayGrid grid = putStrLn $ unlines grid</code></pre>
Remember to add <code>displayGrid</code> to the top of the module within the <code>module Lib</code> parentheses to allow the function to be publically accessed.  

Notice the <code>$</code> replacing the set of parenthesis that would normally be around <code>unlines grid</code>, this is because a set of parenthesis can be replaced by a dollar sign in Haskell, it is purely a cosmetic change and has no other effect on the code or its execution. 

To make sure that we implemented <code>displayGrid</code> correctly, we should test it in the stack GHCI. Once you have entered the stack GHCI, type in <code>displayGrid grid</code> and a nicely formatted Word Game grid should appear on the screen. 

That's it for today folks! Next time we will work on implementing an algorithm to determine if a specified word is located within the grid, regardless of weather it is written horizontally, vertically, forwards, backwards, or any combination of the four. 

## References
<ul>
    <li><a href="https://www.linkedin.com/learning/learning-haskell-programming/the-course-overview?u=2195556">LinkedIn Learning</a></li>
</ul>

## Source Code
<ul>
    <li><a href="https://github.com/GaryZ700/Haskell_Blog/tree/master/WordGame">Click here</a> to access project source code.</li>
</ul>

<b><a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog7.md">Next Post</a></b><br/>
<b><a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog5.md">Previous Post</a></b>
