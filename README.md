# CPSC 354: Haskell Blog 

## 09-01-2020: Day 1

  Hello World!  
  
  Welcome to my Haskell Blog for CPSC 354 at Chapman University. 354 is course designed to teach students the inner workings of programming langauges, and for this course I shall be keeping a running blog of my endevours and projects in the Haskell functional programming language.  
    
  To start off, I mentioned that Haskell was a <i>functional</i> programming language. Now, what does that even mean? Simply put, traditional programming paradigms are ones where code is executed line by line sequentially and memory is modified via an assignment operator and is referred to as an <i>imperative</i> programming style. On the other hand <i>functional</i> programming involves direct evaluation of functions that perform the desired operations and the task of the programmer is to create a series of statements
whose evaluation results in the desired output. 
  
  My first step into Haskell involved installing Haskell and was a very smooth operation overall. Haskell can be downloaded using the instructions from this <a href="https://www.haskell.org/platform/">website.</a>  
  
  The laptop I have runs the Windows operating system, and thus, the following directions will describe the Windows installation procedure. Interestingly enough, Haskall is not downloaded via an executable file, and instead uses the software management platform <a href="https://chocolatey.org/install">Chocolatey.</a> The first step is to install Chocolatey on the Windows machine.  
  
  The Choclately installation is very straightforward, simply start up an adminstrative level Powershell and then enter the following command into the shell: <pre><code>Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex         ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))</pre></code>  
  
  Once Choclately has finished its installation, the next step is to download Haskell itself. This is done by starting up any regular command prompt in Windows, (the Powershell can be terminated once Choclately has finished installing), and entering the following command:  
  <pre><code>choco install haskell-dev  
  refreshenv</code></pre>  
  
  Ta-da! Now Haskell is installed on your system and we will write a simple Fibonacci generator function to generate the nth Fibonacci number.  
  
  First locate a directory where you would like to write your Haskell code. Any folder will do, it can be your Desktop, Documents, or any other folder of your choosing. Just make sure you are able to easily reach the folder you choose via the `cd` command in your command line. Open up a blank file using Notepad or any other text editor of your choice and write the following code into the file: <pre><code>fib 0 = 0
  fib 1 = 1
  fib n = fib (n-1) + fib(n-2)</pre></code>  
  
  Save the file as fib.hs. Note that all Haskell source files will end with the .hs extension. To run your first Haskell program, open up your command line and navigate to the directory where the fib.hs file is located. Once in the same directory as the file, type <code>ghci</code>, which will open up the Haskell command line interface similar to how typing  `python` will open the python interface. Now type `:load fib.hs`. The `:load` command instructs Haskell to load in the specified Haskell file for parsing. Then simply type `fib 10` to generate the 10th Fibonacci number on screen.  

As you can see, the `:load` pulls in a source Haskell file and parses its data, loading its logic into memory and then allowing you, the user, to interactivly call and interact with said function. You can continue to play with the `fib` function you defined, and when you are ready to leave the Haskell shell, type in `:exit`.  

This was my first forrary into the lovely world of Haskell programming, but there will definitly be more to come in the future!

## Refernces 
<ul>
    <li><a href="https://www.haskell.org/">Haskell Website</a></li>
    <li><a href="https://chocolatey.org">Choclatey Website</a></li>
</ul>

## Source Code
<a href="https://github.com/GaryZ700/Haskell_Blog/tree/master/Day1_Code">Click here</a> to access the source code used in this blog post. 

<b><a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog1.md">Next Post</a></b>
