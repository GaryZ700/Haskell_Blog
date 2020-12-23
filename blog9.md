# CPSC 354: Haskell Blog  

## 12-19-2020: It's all Fun & Games till Someone gets Stacked, Part 6

If you have been keeping up with this blog up until now, then congratulations! All of your hardwork is about to pay off when you finish off the word game project! If you haven't been following along then take a look at <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog4.md">Part 1</a>, <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog5.md">Part 2</a>, <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog6.md">Part 3</a>, <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog7.md">Part 4</a>, and <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog8.md">Part 5</a> to make sure you have all of the needed background and code to understand this post. 

## The Main Game

We're at the end folks, just about ready to wrap up this project and call it quits. In this post we will be building up a method to play the game, and then calling the method to play the game in its entirety. Open up Lib.hs and we can get started by adding in the following function: 
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

As you could tell, partial words are detected. Not to mention that if the same word is input twice, it will be counted as giving the user two points for being found twice. In order to deal with this we will need an array of actual words that are in the game, which means the following variable needs to be declared: 
<pre><code>wordsList = ["LAB", "CHEMISTRY", "MATH", "EXPERIMENT", "PHYSICS", "MOLECULES", "SPACE", "HYPOTHESIS", "ENTROPY"]</code></pre>
Now we need a function to determine if a given word is located within a list, and this function is <code>elem</code>. The <code>elem</code> method takes an element along with a list of elements and checks if said element belongs to the list and returns a bool. Thus, we only count a word if is in the game grid, in the words list, and is not in the found word list. This can all be neatly implemented as follows: 
<pre><code>playGame :: IO ()
playGame = do
              displayGame grid 0
              putStrLn "Enter a word in the grid: "
              word <- getLine
              if (findWord grid (map toUpper word) && (elem (map toUpper word) wordsList))
              then if (elem (map toUpper word) foundWords)
                   then putStrLn "Alreadly Found Word"
                   else do
                           putStrLn "Found Word"
                           
              else putStrLn "Did Not Find Word"
</code></pre>
After some testing you should see that this solution ensures that only a complete word match will result in the word being accepted. But, you may also have noticed that the score is not increasing with each correct word. This issue is a bit tricky to solve due to the functional nature of Haskell. If we had been writing this program in Python or Java, we could modify an external variable to keep track of the data for use, but in this case we will need to use a recursive method that will not only cause the game to loop but will also allow use to pass in an ever-changing list of words that have been alreadly guessed by the player. In order to accomplish this goal, we will need to create a <code>gameLogic</code> function and modify the <code>playGame</code> function as follows: 
<pre><code>playGame :: IO ()
playGame = gameLogic [] 0

gameLogic :: [String] -> Integer -> IO ()
gameLogic foundWords score = do
              if (score == 9)
              then putStrLn "Congratulations!! You found all of the words!!\n\n"
              else do
                      displayGame grid score
                      putStrLn "Enter a word in the grid: "
                      word <- getLine

                      if (findWord grid (map toUpper word) && (elem (map toUpper word) wordsList))
                      then if (elem (map toUpper word) foundWords)
                      then do
                              putStrLn "Alreadly Found Word\n\n"
                              gameLogic foundWords score
                      else do
                              putStrLn "Found Word\n\n"
                              gameLogic ((map toUpper word):foundWords) (score + 1)
                      else do
                              putStrLn "Did Not Find Word\n\n"
                              gameLogic foundWords score
</code></pre>

Let's do some more testing! What do you think? Pretty neat huh? We are at the home stretch!! The main game is done and we only have a little bit more coding left before we're done! First of all, notice all of the stars in the grid? They don't look very professional or very clean do they, so let's take them out and replace them with random letter to have a better looking word search. This is what mine looks like after making the change, but feel free to customize yours as you please: 
<pre><code>grid = [ "LABASDFASDFGFDSCBAZE"
            , "CHEMISTRYQWERTYUDFXZ"
            , "ACBVSDAAMSFGVCXZDPSP"
            , "ASDFBHFDGAASDFASECAH"
            , "SELUCELOMATFSRBRVCDY"
            , "GAFWRRTAHRGHABIMXCAS"
            , "GWGECAPSSADFBMXCVBSI"
            , "HYPOTHESISBAELOKJIAC"
            , "NJHGBCTQWXBNPOILAZGS"
            , "OPIJUHTGBATNMLKIJUHA"
            , "ASEGEENTROPYMNBVCFGT"
            ]
</code></pre>
Looking pretty fancy wouldn't you say so? Ok, time to clean up what we are making publicly available since we have finshed testing and the game is done. Change the function names at the top of the page where the parenthesis are to this:
<pre><code>module Lib
    ( playGame
    ) where
</code></pre>
As you can see, we removed all other funcation names except for <code>playGame</code>, this was done since the only function needed outside of the library in order to play the game is <code>playGame</code>, the other functions are only used as support functions for <code>playGame</code> and no longer need to be exposed. Now if we head over to the Main.hs file located in the app folder of the main Word Game directory, and open up said file you should see something along the lines of this: 
<pre><code>module Main where

import Lib

main :: IO ()
main = someFunc
</code></pre>
The contents of your exact file may differ from the one depicted here, but that is perfectly ok. The important things that need to be included are the <code>main</code> function and the <code>import Lib</code> function. Now we can modify <code>main</code> to this: 
<pre><code>main :: IO ()
main = playGame
</code></pre>

The way that the game was designed in our Lib.hs file allows the main to only need to call <code>playGame</code> in order to play the Word Game properly. In order to test that the change to <code>main</code> was sucessful enter the following comand into the terminal while in the Word Game folder: <code>stack run</code>. 

If you are seeing the Word Game, play it and see if you can win! If everything is functioning properly then it is completed!! Congratulations and thenk you so much for sticking through this blog long enough to get to this point. I hope  you have had a fun journey into the wonderful, (but often complicated), world of Haskell and that you decide to keep on coding in functional programming languages. I will end by providing some ideas on different ways you can further extend the Word Game: 
<ul>
    <li>Add code to change the random letters in the grid each time the game is started.</li>
    <li>Allow the user to exit the game without needing to win.</li>
    <li>Add multiple grids that the user can play.</li>
    <li>Add a menu to the game to allow the user to pick which grid to play.</li>
    <li>Add a feature where the program generates a random Word Game grid using a list of provided words.</li>
</ul>

## References 
<ul>
    <li><a href="https://www.linkedin.com/learning/learning-haskell-programming/the-course-overview?u=2195556">LinkedIn Learning</a></li>
    <li><a href="https://stackoverflow.com/questions/2784271/haskell-converting-int-to-string">show</a></li>
    <li><a href="https://stackoverflow.com/questions/22235906/ghci-error-not-in-scope-isupper">toUpper</a></li>
    <li><a href="http://zvon.org/other/haskell/Outputprelude/elem_f.html">elem</a></li>
</ul>

<b><a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog8.md">Previous Post</a></b>
