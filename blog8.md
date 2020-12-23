# CPSC 354: Haskell Blog  

## 12-19-2020: It's all Fun & Games till Someone gets Stacked, Part 5

Hello again fellow Haskellers! Remember to take a look at <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog4.md">Part 1</a>, <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog5.md">Part 2</a>, <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog6.md">Part 3</a> and <a href="">Part 4</a> of this project if you have not alreadly. 

Last post we discussed and implemented horizontal word searching in the grid from right-to-left and from left-to-right, now we are going to figure out how to handle vertical word searching. 

## Vertical Word Searching

Let's start off by creating our function definition in the Lib.hs module: 
<pre><code>findVerticalWord :: [String] -> String -> Bool
</code></pre>
Also remember to add <code>findVerticalWord</code> to the parenthesis at the top of the page to allow the function to be exported from the Lib.hs file.

We alreadly have a very well working horizontal word searching function, but now the question of how do we look for vertical words can be quite tricky since each character would require us to iterate through each column to get all of the letters that could form a single word. In short, we need to search column by column in the grid to detect vertical words. Each column would also need to be reversed in order to detect if the word is written top-to-bottom or bottom-to-top. Unfortunately, due to the nature of a grid in Haskell being a list of lists, it is not the easiest thing to take out an individual column for inspection and to be able to reverse it. But, the builtin Haskell list library does come with a very enticing function called <code>transpose</code>. 

<code>transpose</code> takes a list of lists and then transposes it, meaning that the columns become the rows, and the rows become the columns. In other words, our vertical word searching technique is in essence just to convert the vertical words to horizontal words and then run the transposed grid through the <code>findHorizontalWord</code> function. First we need to import <cold>transpose</code> from the list module by adding the following line to Lib.hs:
<pre><code>import Data.List (transpose)</code></pre>

Luckily, this is as easy as it sounds: 
<pre><code>findVerticalWord :: [String] -> String -> Bool
findVerticalWord grid word = findHorizontalWord (transpose grid) word
</code></pre>

And there we go folks! That wasn't too painful....I hope. Now fire up GHCI with <code>stack ghci</code> and let's do some testing. Try entering in the following lines into GHCI and see what happens: 
<pre><code>findVerticalWord grid "CAT"
findVerticalWord grid "LAB"
findVerticalWord grid "PHYSICS"
</code></pre>

WOW! Looks like it worked on the first go! That's super exciting and means that now we get to move onto the next part. 

## Diagonal Word Searching

Almost done, we just need to find a method to perform diagonal word searching and I'm afraid to say this is going to be our most complicated search method to date. To clarify this method, it will be demonstrated on this grid from the Word Game: 
<pre><code>grid = [ "LAB****************E"
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
As the example in this grid, we will be looking at the word "EXPERIMENTS" that runs diagonally right-to-left starting at the upper left hand corner, and ends on the second to last line of the grid. Hopefully you can locate "EXPERIMENTS" for yourself in the grid, as it will be very helpful to have a firm idea on its location as we proceed from here. 

Ok, you ready for the big trick? Here it is, the words that are written diagonally can be made into vertical words by adding a space to the right in a pyramidal fashion, with no space on the first line, two spaces on the next line, three on the next and so on and so forth. Don't believe me? Let's check it out with "EXPERIMENT":
<pre><code>grid = [ "LAB****************E"
       , " CHEMISTRY*********X*"
       , "  ********M********P*P"
       , "   *********A******E**H"
       , "    SELUCELOM*T****R***Y"
       , "     ***********H**I****S"
       , "      ***ECAPS*****M*****I"
       , "       HYPOTHESIS**E******C"
       , "        ***********N*******S"
       , "         **********T*********"
       , "          *****ENTROPY********"
       ]
</code></pre>
You see it, right? Just by adding spaces in this pyramidal fashion we are able to turn a diagonal word into a vertical word, which we can feed into <code>findVerticalWord</code>. But, there is one caveat, diagnoal words that go from right-to-left will not work with this schme, even though the above left-to-right diagnoal word does. This issue can bee with the following sample grid: 
<pre><code>grid = [ "L***"
       , "*A**"
       , "**B*"
       , "****"
       ]
</code></pre>
<pre><code>grid = [ "L***"
       , " *A**"
       , "  **B*"
       , "   ****"
       ]
</code></pre>
But in order to account for right-to-left diagnoal words, all of the rows can be reversed one by one using the <code>map</code> and <code>reverse</code> functions to turn it into a left-to-right word, making a simple and elegant solution.

With the basic idea out of the way, let's get started on implementing the diagonal word check: 
<pre><code>getDiagonalWord :: [String] -> String -> Bool
</code></pre>
Remember to add <code>getDiagonalWord</code> to the parenthesis at the top of the module in order to allow the function to be used outside of the Lib.hs file. 
Wait, how are we going to add spaces in a pyramidal fashion to the grid in order to accomplish our implementation? If you have been following along with this blog, then a very similar function was implemented for the simple ball project in <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog3.md">this post</a>. Based on the logic described in that older post, the <code>diagToVertical</code> and <code>spaceAdder</code> methods can be devised.

### diagToVertical & addSpace
<code>diagToVertical</code> will take a grid and an integer that should be passed in as zero to represent how many spaces the first line should receive. From there, <code>addSpace</code> adds in the specified number of spaces to each line as directed by the <code>diagToVertical</code> function. Both functions will be recursive, reconstructing the entire grid with the extra spaces. The functions are as follows: 
<pre><code>diagToVertical :: [String] -> Integer -> [String]
diagToVertical (s:grid) spaces = (addSpace s spaces): (diagToVertical grid (spaces + 1))
diagToVertical [] spaces = []

addSpace :: String -> Integer -> String
addSpace s 0 = s
addSpace s spaces = ' ' : (addSpace s (spaces - 1))
</code></pre>
These functions do not need to be added for public export since they will only be used internally to aid the <code>findDiagWord</code> function, which will need to be added to the parenthesis at the top of the module to allow for public export. <code>findDiagWord</code> takes in the grid and word to look for, and then uses <code>diagToVertical</code>  in order to convert diagonal words to vertical words which can then be passed into <code>findVerticalWord</code>. The implementation is as follows: 
<pre><code>findVerticalWord :: [String] -> String -> Bool
findVerticalWord grid word = findHorizontalWord (transpose grid) word
</code></pre>

Now that we have our implementation, let's get to testing! Try out the following lines of code in <code>stack ghci</code> and see if we get the expected results: 
<pre><code>findDiagWord grid "EXPERIMENT"
findDiagWord grid "LAB"
findDiagWord grid "CAT"
</code></pre>
Another working method!!

Now, let's tie everything together by making a function that given a word to search for, will use all of the above methods to search for the word. This function will be needed since when a player is playing the word game they should be able to simply type in the word to determine if it is on the board as opposed to needing to know if it is a horizontal, vertical, or diagonal word. 

The function will be named <code>findWord</code> and will use an if statement to determine if the word has been found as either horizontal, vertical, or diagonal. If the word is found, the method will return true, otherwise it will return false. Here is the implementation: 
<pre><code>findWord :: [String] -> String -> Bool
findWord grid word = if (findHorizontalWord grid word)
                        then True
                     else if (findVerticalWord grid word)
                        then True
                     else if (findDiagWord grid word)
                        then True
                     else False
</code></pre>

Now let's test it and see if it works. Enter the following commands into <code>stack ghci</code>:
<pre><code>findWord grid "CAT"
findWord grid "MOUSE"
findWord grid "LAB"
findWord grid "EXPERIMENT"
findWord grid "SPACE"
findWord grid "HYPOTHESIS"
findWord grid "MOLECULES"
findWord grid "ENTROPY"
findWord grid "PHYSICS"
findWord grid "MATH"</code></pre>

It's done! The core of our word search feature is 100% done!! Tune in again next post to see how all of this will be wrapped up into a final playable game for the player. 

## References 
<ul>
    <li><a href="https://www.linkedin.com/learning/learning-haskell-programming/the-course-overview?u=2195556">LinkedIn Learning</a></li>
</ul>

<b><a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog9.md">Next Post</a></b><br/>
<b><a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog7.md">Previous Post</a></b>
