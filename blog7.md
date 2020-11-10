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

Although this function meets our needs perfectly, there is still an issue to solve, namely that 

