# CPSC 354: Haskell Blog  

## 12-19-2020: It's all Fun & Games till Someone gets Stacked, Part 5

If you have been keeping up with this blog up until now, then congratulations! All of your hardwork is about to pay off when you finish off the word game project! If you haven't been following along then take a look at <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog4.md">Part 1</a>, <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog5.md">Part 2</a>, <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog6.md">Part 3</a>, <a href="">Part 4</a>, and <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog8.md">Part 5</a> to make sure you have all of the needed background and code to understand this post. 

## The Main Game

We're at the end folks, just about ready to wrap up this project and call it quits. In this post we will be building up a method to play the game, and then calling the method to paly the game in its entirety. Open up Lib.hs and we can get started by adding in the following function: 
<pre><code>playGame :: IO ()
playGame = displayGrid grid</code></pre>
We're going to take this slowly. So the first step is to simply display the grid onto the screen and from there we will keep moving ahead. Remember to place the <code>playGame</code> method in the top where the parenthesis are in order to allow the function to be exported outside of the library. 

Run <code>stack ghci</code> and then enter in <code>playGame</code>. Just like that, the game board is displayed onto the screen, but that isn't too fun is it? We need to be able to add in a score feature. Create a new function as follows that will be used to display the grid with an associated score as well: 
<pre><code>displayGame :: [String] -> Integer -> IO ()
displayGame grid score = do 
                       displayGrid grid
                       putStrLn ("Score: " ++ (show score) ++ "/9\n\n")
                       </code></pre>
<code>displayGame</code> takes in a game grid along with the current score and displays the grid along with said score out of the total possible score. The total score is hard-coded in this example for simplicity, since the game's words and grid were not meant to dynamically change over time, but creating a dyanmically updating word game would make for an interesting spin-off project. Also note that the <code>show</code> function is used to convert an integer to a string. <code>displayGame</code> does not need to be exported from the library since it will be directly invoked by <code>playGame</code>, and with that <code>playGame</code>, should be modified as follows: 
<pre><code>playGame :: IO ()
playGame = displayGame grid 0
</code></pre>

Now execute <code>playGame</code>, and bingo, there is the game board along with the score being printed out onto the screen. Now we need to add in some user interaction to make the game a bit more exciting than it is now. The <code>playGame</code> function can be modified as follows to add in user-input functionality:
<pre><code>playGame :: IO ()
playGame = do
              displayGame grid 0
              putStrLn "Enter a word in the grid: "
              word <- getLine
              if (findWord grid (map toUpper word))
              then putStrLn "Found Word"
              else putStrLn "Did Not Find Word"
</code></pre>

Note, <code>map toUpper word</code> ensures that the word being passed to <code>findWord</code> is all uppercase. In order to make use of <code>toUpper</code> the following import is needed: <code>import Data.Char (toUpper)</code>. Test <code>playGame</code> and try searching for some of the words what happens? In particular pay special attention to what happens if partial words are entered such as, "phys" or "mat". 

As you could tell, partial words are detected. Not to mention that if the same word is input twice, it will be counted as giving the user two points for being found twice. In order to deal with this we will need an array of actual words that are in the game, as well as an array of words that the player has alreadly found. Create these empty arrays as such: 
<pre><code>wordsList = ["LAB", "CHEMISTRY", "MATH", "EXPERIMENT", "PHYSICS", "MOLECULES", "SPACE", "HYPOTHESIS", "ENTROPY"]
foundWords = []</code></pre>
Now we need a function to determine if a given word is located within a list, and this function is <code>elem</code>. The <code>elem</code> method takes an element along with a list of elements and checks if said element belongs to the list and returns a bool. Thus, we only count a word if is in the game grid, in the words list, and is not in the found word list. This can all be neatly implemented as follows: 
<pre><code></code></pre>

## References 
<ul>
    <li><a href="https://www.linkedin.com/learning/learning-haskell-programming/the-course-overview?u=2195556">LinkedIn Learning</a></li>
    <li><a href="https://stackoverflow.com/questions/2784271/haskell-converting-int-to-string">show</a></li>
    <li><a href="https://stackoverflow.com/questions/22235906/ghci-error-not-in-scope-isupper">toUpper</a></li>
    <li><a href="http://zvon.org/other/haskell/Outputprelude/elem_f.html">elem</a></li>
</ul>

<b><a href="">Next Post</a></b><br/>
<b><a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog8.md">Previous Post</a></b>
