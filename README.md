# CPSC 354: Haskell Blog 

## 09-01-2020
<br>
  Hello World! <br>
  Welcome to my Haskell Blog for CPSC 354 at Chapman University. 354 is course designed to teach students the inner workings of programming langauges, and for this course I shall be keeping a running blog of my endevours and projects in the Haskell functional programming language. <br>
  Before I start with the first step in my journey to learning and deveoping in Haskall. 
  <br>
  To start off, I mentioned that Haskell was a <i>functional</i> programming language. Now, what does that even mean? Simply put, traditional programming paradigms where code is executed line by line sequentially and meomory is modified via an assignment operator follows the idea of a <i>imperative</i> programming style. On the other hand <i>functional</i> programming involves direct evaluation of functions that perform the desired operations and the task of the programmer is to create a statement that evaluates functions in the correct order to produce the desired functionality. <bn>
  My first step into Haskell involved installed Haskell and was a very smooth operation overall. Haskell can be downloaded using the instructions from the it <a href="https://www.haskell.org/platform/">website.</a><br>
  The laptop I have runs the Windows operating system, and thus, the following directions will describe the Windows installation procedure. Interestingly enough, Haskall is not downloaded via an executable file, and instead uses the software management platform <a href="https://chocolatey.org/install">Choclatey.</a> The first step then is to install Choclately on the Windows machine. <br>
  The Choclately installation is very straightforward, simply start up an adminstrative level Powershell and then enter the following command into the shell: `Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))`
