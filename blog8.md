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

##
