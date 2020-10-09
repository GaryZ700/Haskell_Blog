# CPSC 354: Haskell Blog  

## 10-08-2020:  It's all Fun & Games till Someone gets Stacked, Part 2

If you haven't alreadly checked out Part 1 of this post, make sure to do so <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog4.md">here.</as>, as this post assumes you already have the WordGame directory created and the Haskell Stack installed. 

#package.yaml
Let start by navigating to the WordGame folder, and opening up the package.yaml file in a text editor. You should see somthing similar to this near the top of the file:
<pre><code>name:                words
version:             0.1.0.0
github:              "githubuser/words"
license:             BSD3
author:              "Author name here"
maintainer:          "example@example.com"
copyright:           "2020 Author name here"
</code></pre>
These values can be changed to meet your actual information or can be left as is. It is important to note that the Haskell Stack allows for projects to be built with Github in mind. Feel free to look further down this file and see what other options are configurable. You should note a section stating <code>executable</code> followed by the name of the project-exe. The term following <code>executable</code> refers to the name of the executable to be built, and can be changed to your pleasing.

#Testing
Another very nice featuer of the Haskell Stack is automated unit testing. Enter the test folder and from there open up the Spec.hs file to set up unit testing. You should a main function defined within Spec.hs that prints out that testing has not been implemented. Well, looks like we've got to implement it then. Here is the general code for any form of unit testing using Haskell: 
<pre><code>import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "1st Unit Test" $ do
      it "Should verify the output of a Haskell Expression" $ do
        someFunc `shouldBe` "unitTest"
</code></pre>
Let's take a look at what this code is doing line by line. <code>import Test.Hspec</code> means that Haskell should import the Haskell unit testing library to allow testing. Lib.hs is imported to provide accesss to its methods here for unit testing. A main function is declared here to signal the entery point of test code execution. the hspec function is called in order to start the unit testing process. It it followed by <code>$ do</code> to indicate each 
