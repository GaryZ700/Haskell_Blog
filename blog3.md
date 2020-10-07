# CPSC 354: Haskell Blog  

## 10-06-2020: It's all Fun & Games till Someone gets Stacked

Today marks the begining of a new dawn, a new era, a new epoch!! (Well, not really in the real world, but certainly for this blog!)

Today is the day we start to build our first real project. No longer are we limited to basic and contrived Haskell examples, today we start coding for real. The project will be to develop a simple text-based word search gamne in Haskell. This project is based off of the one found <a href="https://www.linkedin.com/learning/learning-haskell-programming/creating-a-project-with-stack?u=2195556">here</a>, and will require use of the Haskell stack. 

## The Stack 
What is the Haskell stack? It is a set of tools that provides a mthod to download packages for Haskell, (similar to what pip is to Python), creating projects and managing them like an IDE, testing a project, and benchmarking the performance of a project. 

Before we can take advantage of the stack, it has to be downloaded first. Since I am using a Windows laptop, the following instructions apply for a Windows machine. Download the Haskell tool stack from <a href="https://docs.haskellstack.org/en/stable/README/"></a>. There will be a link to an executable binary for Windows, as well as installation instructions for other operating systems. 

## Starting a New Project
Once the Haskell stack has downloaded and been installed, it's time to create a new project for our word game. Enter <code>stack new WordGame</code> to create a new folder containing a files to start developing a Haskell project. <code>cd</code> into the new directory. 
