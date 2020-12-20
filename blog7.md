# CPSC 354: Haskell Blog  

## 11-08-2020:  It's all Fun & Games till Someone gets Stacked, Part 3

Before we start, remember to take a look at <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog4.md">Part 1</a>, <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog5.md">Part 2</a> and <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog6.md">Part 3</a> of this blog post to ensure you have the needed understanding of the project and the Haskell stack. 

Resuming from time we have our game grid ready to and displaying onto the screen, and our next task is to be able to determine if a certain word provided by the user is contained within the grid of letters. The different words contained within the grid can be written either horizontally, vertically, or diagonally, as well as forwards and backwards. To best tackle this problem we will break it down into its separate pieces and from there create the complete solution to this issue. 

## Horizontal Words
Here is the grid that I am working with, but your grid can be different and use a different set of words: 
<pre><code>LAB****************E
CHEMISTRY*********X*
********M********P*P
*********A******E**H
SELUCELOM*T****R***Y
***********H**I****S
***ECAPS*****M*****I
HYPOTHESIS**E******C
***********N*******S
*****ENTROPY********</code></pre>
Luckily for us, Haskell has a built in method that is capable of searching for words in an array such as the one presented above, this is the <code>isInfixOf</code> function which essentially checks if a string exists within a another string. In order to use this method, first it most be imported from the Data.List package by adding the following command to the Lib.hs file: <code>import Data.List (isInfixOf)</code>. The method functions by taking in the parameter of the string being searched for, followed by the parameter of the string to serach in. 

Although this function meets our needs perfectly, there is still an issue to solve, namely that it will be unable to deal with words that are spelled backwords, such as molecules in this grid. Luckily, there is an easy fix for this which is to use the <code>reverse</code> function. <code>reverse</code> takes a string and reverses it. It seems as if we only need to reverse the line we are looking at, and then use the <code>isInFixOf</code> function. That is the general gist of our solution, but we are not yet entirely down. If we were working in Python, we could use a for loop to iterate through all of the text lines in the grid and then run <code>isInFixOf</code> and <code>reverse</code> to check for both the forward and reverse cases of each word. But, since this is Haskell, there is no conventional for loop a-la Python or Java. Instead, we will need to take advantage of the <code>map</code> function, which takes a function and a list of items as a its inputs. <code>map</code> takes the function given to it and runs it on all of the items in its second parameter and then returns a list with the all of the items in the original list having been operated on by the method. 

As an example, run <code>ghci</code> in your WordGame directory and try out the following code to see what happens: 
<pre><code>reverse grid
map reverse grid</code></pre>
As you can see, <code>reverse grid</code> returned the oringinal grid with the elements in reverse order, but the strings still reading as they were orginally. On the other hand, running <code>map reverse grid</code> returns the grid with each line of the grid containing reversed strings. This is due to the nature of the <code>map</code> function, it takes a function and maps it to each of the elements in a list. 

Now, with this we are able to start implementing a function to handle searching for words in the grid that are both written backwards and forwards. Open up Lib.hs and add in the following function definition: 
<pre><code>findHorizontalWord :: [String] -> String -> Bool
</code></pre>

Also, remember to add <code>findHorizontalWord</code> inside of the paranthesis near <code>module Lib</code> to allow the funtion to be made publicly avaible out of the class. Now, let's figure this out. We know we need to have this function both search for horzontal words that are written left-to-right and right-to-left. Maybe we could use a boolean operator to check if the word is written either backwards or forwards in the grid? Let's give it a go with this method: 
<pre><code>findHorizontalWord :: [String] -> String -> Bool
findHorizontalWord grid word = map (isInfixOf word) grid
</code></pre>

