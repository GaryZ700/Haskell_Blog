# CPSC 354: Haskell Blog  

## 12-19-2020: It's all Fun & Games till Someone gets Stacked, Part 5

Hello again fellow Haskellers! Remember to take a look at <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog4.md">Part 1</a>, <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog5.md">Part 2</a>, <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog6.md">Part 3</a> and <a href="">Part 4</a> of this project if you have not alreadly. 

Last post we discussed and implemented horizontal word searching in the grid from right-to-left and from left-to-right, now we need are going to figure out how to handle vertical word searching. 

## Vertical Word Searching

Once again, let's start off by creating our function definition in the Lib.hs module: 
<pre><code>findVerticalWord :: [String] -> String -> Bool
</code></pre>
Also remember to add <code>findVerticalWord</code> to the parenthesis at the top of the page to allow the function to be exported from the Lib.hs file.

We alreadly have a very well workiing horizontal word searching function, but now the question of how do we look for vertical words can be quite tricky since each character would require us having to iterate through each row to get together all of the letters to form a vertical word. In other words, we need to search column by column in the grid to detect vertical words. Each column would also need to reversed in order to detect if the word is written top-to-bottom or bottom-to-top. Unfortunately, due to the nature of a grid in Haskell being a list of lists, it is not the easiest thing to take out an individual column for inspection and to be able to reverse it. But, the Haskell language list library does come with a very enticing function called <code>transpose</code>. 

<code>transpose</code> takes a list of lists and then transposes it, meaning that the columns become the rows, and the rows become the columns. In other words, our vertical word searching technique is in essence just to convert the vertical words to horizontal words and then run the transposed grid through the <code>findHorizontalWord</code> function. First we need to import the <colde>transpose</code> function from the list module by adding the following line to Lib.hs:
<pre><code>import Data.List (transpose)</code></pre>

Luckily, this is as easy as it sounds: 
<pre><code>findVerticalWord :: [String] -> String -> Bool
findVerticalWord grid word = findHorizontalWord (transpose grid) word
</code></pre>

And there we go folks! That wasn't too painful, I hope. Now fire up GHCI with <code>stack ghci</code> and let's do some testing. Try entering in the following lines into GHCI and see what happens: 
<pre><code>findVerticalWord grid "CAT"
findVerticalWord grid "LAB"
findVerticalWord grid "PHYSICS"
</code></pre>

WOW! Looks like it worked on the first go! That's super exciting and means that now we get to move onto the next part. 

## Diagonl Word Searching

Almost done, we just need to find a method to perform diagonal word searching and I'm afraid to say that this is going to be our most complicated search method to date. To make this method more clear, it will be demonstrated line by line on this grid from the Word Game: 
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
As the example in this grid, we will be looking at the word "EXPERIMENTS" that runs diagonally right-to-left startijng at the upper left hand corner, and whose final "T" is shared with "ENTROPY". Hopefully you can locate "EXPERIMENTS" for yourself in the grid, as it will be very helpful to have a firm idea on its location as we proceed from here. 

Ok, you ready for the big trick? Here it is, the words that are written diagonally can be made into vertical word by adding a space to the right in a pyramidal fashion, with no space on the first line, two spaces on the next line, three on the next and so on and so forth. Don't believe me? Let's check it out with "EXPERIMENT". 
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
You see it, right? Just by adding spaces in this pyramidal fashion we are able to turn a diagnoal word into a vertical word, which we can then feed into our <code>findVerticalWord</code> function. But, there is one caveat, diagnoal words that go from right-to-left will not work with this schme, even though the above left-to-right diagnoal word does. This issue can bee with the following sample grid: 
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
Wait, how are we going to add allow add spaces in a pyramidal fashion to the grid in order to accomplish our implementation? If you have been following along with this blog, then a very similar function was implemented in a simple ball project in <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog3.md">this post</a>. Based on the logic described in that older post, the <code>diagToVertical</code> and <code>spaceAdder</code>.

### diagToVertical & addSpace
<code>diagToVertical</code> will take a grid and an integer that should be passed in as zero to represent how many spaces the first line should receive. From there, the <code>addSpace</code> function adds in the specified number of spaces to each line as directed by the <code>diagToVertical</code> function. Both functions will be recursive, reconstructing the entire grid with the extra spaces. The functions are as follows: 
<pre><code>diagToVertical :: [String] -> Integer -> [String]
diagToVertical (s:grid) spaces = (addSpace s spaces): (diagToVertical grid (spaces + 1))
diagToVertical [] spaces = []

addSpace :: String -> Integer -> String
addSpace s 0 = s
addSpace s spaces = ' ' : (addSpace s (spaces - 1))
</code></pre>
These functions do not need to be added for public export since they will only be used internall to aid the <code>findDiagWord</code> function, which will need to be added to the parenthesis at the top of the module to allow for public export. The <code>findDiagWord</code> function takes in the grid and word to look for, and then uses the <code>diagToVertical</code> function in order to convert diagnoal words to vertical words which can then be passed into the <code>findVerticalWord</code> function. The implementation is as follows: 
<pre><code>findVerticalWord :: [String] -> String -> Bool
findVerticalWord grid word = findHorizontalWord (transpose grid) word
</code></pre>
