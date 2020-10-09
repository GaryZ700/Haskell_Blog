# CPSC 354: Haskell Blog  

## 10-08-2020: It's all Fun & Games till Someone gets Stacked, Part 1

Today marks the begining of a new dawn, a new era, a new epoch!! (Well, not really in the real world, but certainly for this blog!)

Today is the day we start to build our first real project. No longer are we limited to basic and contrived Haskell "ball simulations", today we start coding for real. The project will be to develop a simple text-based word search gamne in Haskell. This project is based off of the one found <a href="https://www.linkedin.com/learning/learning-haskell-programming/creating-a-project-with-stack?u=2195556">here</a>, and will require use of the Haskell stack. 

## The Stack 
What is the Haskell stack? It is a set of tools that provides a mthod to download packages for Haskell, (similar to what pip is to Python), creating projects and managing them like an IDE, testing a project, and benchmarking the performance of a project. 

Before we can take advantage of the stack, it has to be downloaded first. Since I am using a Windows laptop, the following instructions apply for a Windows machine. Download the Haskell tool stack from <a href="https://docs.haskellstack.org/en/stable/README/"></a>. There will be a link to an executable binary for Windows, as well as installation instructions for other operating systems. 

## Starting a New Project
Once the Haskell stack has downloaded and been installed, it's time to create a new project for our word game. Enter <code>stack new WordGame</code> to create a new folder containing a files to start developing a Haskell project. <code>cd</code> into the new directory. 

You will notice that the WordGame folder contains plenty of files and folders. The most important data that we will concern ourselves with resides within the app folder and the src folder. To start off, let's examine the app folder. The app folder contains the main code to be used for this application. You will notice a file named Main.hs in this folder, and open it up in your text editor to view the following code: 
<pre><code>module Main where

import Lib

main :: IO ()
main = someFunc
</code></pre>
There are two lines here that might raise some eyebrows, namely <code>module Main where</code> and <code>import Lib</code>. The <code>module Main where</code> line is an explicit definition that states that this file contains the main module that is the entery point of application execution. This line can be included or ommited, as the GHC can automatically determine if it is dealing with the main expression even if this line is not included. <code>import Lib</code> is an import statement along the lines of an import in Python or an include in C/C++. It imports an external code library in another file and exposes its methods and expressions to this file. Also, take note that the main expression calls another expression by the name of <code>someFunc</code>.

If we head out of the app folder and navigate to the src folder where we can see a file by the name of Lib.hs. This is the Haskell being imported by Main.hs. Let's open this file up, and take a look inside. You should see the following code: 
<pre><code>module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
</code></pre>
Ok, we now know where the <code>someFunc</code> function from Main.hs is coming from. But what is this <code>module Lib</code> thing that is at the top of the file? In Haskell, any external library file must contain the header <code>module moduleName (func1, func2, func3) where</code>, where <code>func, func1, func2</code> are the names of expressions to be made publically available to the importing file. If the function name is not included in this module decleration, then it is considered private and will not be accessible by the importing file. 

Alright, now that we have an idea of the main files we will be working in for this project, let's see about running this project. Exit the src folder back to the main project folder. Enter the command <code>stack build</code> to build the project, and then enter <code>stack ghci</code> to launch the interactive ghc terminal with the project code already loaded into it. Type <code>main</code> to see what running the main function does, also enter <code>someFunc</code> to run the function contained in the Lib.hs file. Enter <code>:quit</code> to exit the interactive terminal. 

#Basic Stack Commands
Below follow a list of basic Haskell Stack commands you should familiarize yourself with: 
<ul>
  <li><code>stack build</code> - Builds the project into an executable file by the name of "projectName-exe"</li>
  <li><code>stack ghci</code> - Opens the ghci terminal and loads in the data defined in the project files</li>
  <li>stack exe exeFileNAme</li> - Executes the executable file built by <code>stack build</code>, the file name will normally be of the form "projectName-exe"
</ul>
Let's try to execute the project from the stack directly, enter <code>stack exe WordGame-exe</code>. The program should then execute on screen. 

Allrighty! That's it for this time, in the next installment we will continue to explore more of the stack's features. 
