# CPSC 354: Haskell Blog  

## 10-08-2020:  It's all Fun & Games till Someone gets Stacked, Part 2

If you haven't alreadly checked out Part 1 of this post, make sure to do so <a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog4.md">here</a>, as this post assumes you already have the WordGame directory created and the Haskell Stack installed. 

# package.yaml
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

# Testing
Another very nice feature of the Haskell Stack is automated unit testing. Enter the test folder and from there open up the Spec.hs file to set up unit testing. You should a main function defined within Spec.hs that prints out that testing has not been implemented. Well, looks like we've got to implement it then. Here is the general code for any form of unit testing using Haskell: 
<pre><code>import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "1st Unit Test" $ do
      it "Should verify the output of a Haskell Expression" $ do
        someFunc `shouldBe` "unitTest"
</code></pre>
Let's take a look at what this code is doing line by line. <code>import Test.Hspec</code> means that Haskell should import the Haskell unit testing library to allow testing. Lib.hs is imported to provide accesss to its methods here for unit testing. A main function is declared here to signal the entry point of test code execution. The hspec function is called in order to start the unit testing process. It it followed by <code>$ do</code> to indicate each sublevel of code that provides further information on the testing process. Under the first <code>$ do</code> the <code>describe</code> keyword is used along with a string that describes this specific unit test. The 2nd <code>$ do</code> uses the <code>it</code> command followed by a string that describes what the unit test should do in more specifically. The 3rd <code>$ do</code> is used with the name of the function to test, followed by <code>`shouldBe`</code> and the expected output of said function. In this example, the someFunc returns an empty IO object. Unfortunately, unit testing can not test for this, but we can modify the function to instead return a string, allowing us to ilustrate the testing feature in stack. 

The new Lib.hs code is: 
<pre><code>module Lib
    ( someFunc,
      someFunc2
    ) where

someFunc :: String
someFunc  = "Cat"

someFunc2 :: String -> String
someFunc2 s = s
</code></pre>
This is a trival change to the function resulting in it returning a string that was passed into it. This was done in order to also ilustrate how to test multiple functions, as well as functions that use inputs.

The new Main.hs code is: 
<pre><code>module Main where

import Lib

main :: IO ()
main = putStrLn (someFunc)
</code></pre>

And here is the updated test code: 
<pre><code>import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "1st Unit Test" $ do
      it "Should verify the output of a Haskell Expression" $ do
        someFunc `shouldBe` "Cat"
        someFunc2 ("Cat") `shouldBe` "Mat"
</code></pre>
As you can see, additional functions to test can simply be put one after the other below the <code>it</code> line. You should also notice that the line <code>someFunc2 ("Cat") \`shouldBe\` "Mat"</code> is designed to fail. The <code>func2</code> function simply outputs its input, thus the function will output <code>"Cat"</code> not <code>"Mat"</code> as we are telling the code to expect. This is done to show what happens when a unit test fails. 

Ok, looks like we're ready to start testing! But...not just yet. The <code>Test.Hspec</code> module is not included by default in stack or the project and we need to explicitly tell the Stack that we need this module. This is done by adding a single line of code to the WordGame.cabal file located in the main WordGame directory. Open that file in an editor of your choice, and navigate to the bottom of the document where you should see a section that looks like this: 
<pre><code>test-suite WordGame-test
  type: exitcode-stdio-1.0
  main-is: Spec.hs
  other-modules:
      Paths_WordGame
  hs-source-dirs:
      test
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      base >=4.7 && <5
    , WordGame
  default-language: Haskell2010</code></pre>
And modify the <code>build-depends</code> section to contain <code>hspec</code>:
<pre><code>test-suite words-test
  type: exitcode-stdio-1.0
  main-is: Spec.hs
  other-modules:
      Paths_words
  hs-source-dirs:
      test
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      base >=4.7 && <5
    , words
    , hspec
  default-language: Haskell2010</code></pre>
Adding <code>hspec</code> to the cabal file instructs the Stack that it requires the Test.Hspec library, and it will take care of the rest from there. Alright then, we are ready to test!! Enter <code>Stack test</code>, and the second test fails as expected. Try playing around with Spec.hs and see what other tests you can come up with.

Until next, in Part 3 we finally get to start building up our word game now that we have a basic understanding of the stack!
