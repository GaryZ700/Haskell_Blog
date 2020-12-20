# CPSC 354: Haskell Blog  

## 09-08-2020: The Basics 

Before diving into creating programs and functions with Haskell, I would like to take some time to discuss the basics of Haskell and some of its features and syntax in an isolated environment before utilizing said syntax and features to build small programs.  


The information contained within this blog post originates from Haskell training I receved at <a href="https://www.linkedin.com/learning/learning-haskell-programming/the-course-overview?resume=false&u=2195556">LinkedIn Learning's Haskell course.</a>

## GHCI & Simple Operations

GHCI stands for the Glasgow Haskell Complier Interactive. Similar to C++ and Java, Haskell is a compiled language that generates an executable file for your output system of choice. But, Haskell also has an interesting feature where it is able to run as an interpretative language in an interactive shell similar to Python. I will start off by discussing the general commands and functionality available in the interactive GHCI terminal.  

To start GHCI, simply type <code>ghci</code> in your terminal or command prompt. The GHCI terminal will then be brought up, and Haskell commands can be entered and ran. As an example, type in the following commands into GHCI to get a feel for the basic mathematical operations of the language: 
<pre><code>5 + 5
10.5 - 0.5 
16 * 2.0
32 / -4
2 ** 2 
25 ** 0.5
2 ** (-1)
(2 + 2) ** (2 - 3)</code></pre>
Basic operations behave similary as to how they function in other languages such as Python, Java, and C++. One important note regarding numbers and aritmatic experissions is that fractional values such as 0.5, 0.25, and 0.912 can not be written as <code>.5</code>, <code>.25</code> or <code>.25</code>. In other words, decimal values must include the zero before the decimal point in order to be considered valid. 

Below follows a list of logical operators available in Haskell, and can be typed into the GHCI for practice:  

<pre><code>5 == 5 
5 /= 1
5 > 2
2 >= 5
2 < -1 
-1 <= -1
True && False 
True || False 
</code></pre>  
  
 Some important points to notice are that not equal is not <code>!=</code>, but instead uses <code>/=</code>. In addition, the boolean values of true and false are spelled with a capital first letter.  
  
 The next step in examining Haskell's syntax is to introduce variable declarations. Variables are declared the same as in nearly every other coding langauge: 
<pre><code>x = 5
y = -2 
var_name = (2*5) ** 2
variableName = y - z
y2 = 1000 / 0.5
</code></pre>  
It should be noted that semicolons are not required in Haskell similar to Python, and differing from C and Java. Semicolons can be used to run multiple commands on the same line, such as in the following code where the variables <code>a</code>, <code>b</code>, and <code>c</code> are set to numerical values, <code>a=1; b=2; c=3;</code>.
 ## Data Types  
 
 Variable data types are set dynamically at variable assignment, again similar to Python. To check the type of a variable or object in Haskell type <code>:type</code> into the GHCI. The <code>:</code> means that the following command is not a part of the Haskell command library, but instead is a feature specific only to the GHCI. Below follows a list of common Haskell data types: 
 <pre><code>Int
Float
Double
Bool
[a]
Char
[Char]
String
Num a => a
Fractional a => a
</pre></code>
 
 As can be noted there are familiar types from other languages and others that are not quite as familar. I will be delving into the details of each type below. 
 
 ## Int, Float, Double, & Bool  
 There's not much to say about these types, they behave exactly as one would expect. Int are standard integers, floats are decimal values that are not quite as precise as doubles, but use less memory. Bools are true and false as expected.  
 
 ## Char
 Chars are the same as in C++, and are defined in the same manner, by a letter enclosed by single quotes. Ex. <code>'c'</code>. Using a list, strings of characters can be generated similar to how cstrings are arrays of chars.
 
 ## [a], Lists  
<code>[a]</code> represents a list in Haskell, and can be declared as follows, <code>list = [1, 2, 4, 5, 100]</code>. Lists are only allowed to contain elements of the same type, but can contain any data type so long as all of the elements are of the same type. Thus, <code>[2.2, 4.4, 8.8, 10]</code> would be a valid list since all values are numbers, while <code>[500, True, 200]</code> would be invalid since numbers and bools can not be mixed in the same list.  

Unlike most other programming languages, Haskell lists do not support indexing. Instead, Haskell makes use of mathematical notation for interacting with lists via the colon operator. The list <code>[2]</code> can be built by using the following command, <code>2:[]</code>. The colon also allows for appending to the start of a list, <code>1:[2]</code>. The colon operator can be repeated as many times as desired to create any valid list in one line, <code>1:2:3:4:100.1:2:[]</code>, and also to append any number of elements to the front of a list. Elements can be appended to a list using the <code>++</code> operator as follows, <code>[2, 1] ++ [-1]</code>.

In order to access the start of a list, the head function must be used, <code>head [1, 2, 4, 5]</code> will return 1. To get the other elements in a list the tail function must be used, <code>tail [1, 2, 4, 5]</code>, which will return <code>[2, 4, 5]</code> on which head can be called again to acquire 2. Another function which can be called on a list is the length function, which returns the length of the list, <code>length [1,2,3]</code>.

Lists can be automatically created to contain a patterned set of values by using the following syntax: 
<pre><code>[1,2..10]
[1,4..10]
[3,9..100]
['a','b'..,'z']
</code></pre>
Only two starting values can be provided to establish the pattern, followed by the double dots and the ending value for the list.

## [Char], String
Strings can be written using double quotations simmilar to other languages, <code>"This is a Haskell String"</code>.
Strings in Haskell are considered to be lists of Chars, meaning that list operations can also be applied to strings such as the ones that follow below: 
<pre><code>head "Cedar"
tail "Cedar"
length "Birch"
['M', 'A', 'P', 'L', 'E']
'O':'a':'k':[]
'N' : 'u' : 't' : ['W', 'o', 'o', 'd']
'W' : 'a' : 'l' : "Nut"
"Beach" ++ "Wood"
</pre></code>  
Interestingly, even when a string is directly typed into the GHCI as a list of chars, it still is displayed on screen as a string in double quotation marks. If the <code>:type</code> command is called on a string, it can be seen that its internal type is <code>[Char]</code>, a list of chars. Now, there is also a type that is distinctly of type <code>String</code>. The details of this type can be seen using the <code>:info</code> which provides lower-level implementation details and information regarding data types. The command <code>:info String</code> shows that <code>type String = [Char]</code>, in other words the string type in Haskell in simply an alias for <code>[Char]</code>. 

## Data Types

Custom data types can be defined in Haskell via the type command. For the sake of brevity in this post, I will only discuss how to create aliases for alreadly existing types. In order to create an alias for the <code>double</code> data type the following code can be used: <code>type FloatAlias = Float</code>. All type names must start with a capital name otherwise the complier will raise an error, and this procedure can be applied to any data type in Haskell to create an alias. A more in depth discussion on custom data types will be presented in a later blog post.

## Functions  

Function definitions in Haskell are strongly based upon abstract mathematical theory. All functions in Haskell are typed such that a function takes an input from one data type and transforms it to another type, even if the transformation results in the same data type. The domain/range declaration of the function can be ignored and Haskell will do its best to auto-assign an input and output type to the function. 
<pre><code>
f x = 2*x
:type f 
f 2
f 1.1
</code></pre>
The type of the function f is <code>Num a => a -> a</code>, meaning that the function takes a number <code>a</code> as input, and returns another number as <code>a</code>.  And of course, <code>f(2) = 2</code> and <code>f(1.1) = 2.2</code> All functions in Haskell are defined as follows, <code>functionName arg1, arg2, arg3 = some_operation(s)</code>. The type of a function can be explicitly declared as follows: 
<pre><code>f2 :: Int -> Int
f2 x = 2*x
:type f2
f2 2
f2 1.1
</code></pre>
Note that the type of this function is now exactly as it was specified in its definition. The <code>::</code> operator is used to assign a type to functions and objects. Also, note how <code>f 1.1</code> raises an error since 1.1 is a floating-point number as opposed to an integer. Thus, since <code>f2</code> is defined to only work on integers an error is raised due to Haskell's strict mathematical definition of functions. But, then how is <code>f</code> able to work with both integers and floats? Simply put, <code>f</code> takes inputs of Num type and returns values of Num type. The Num type is an alias for all types of numbers, doubles, floats and integers and thus allows all numbers to be accepted into a function.  

In order to exit the GHCI terminal, enter <code>:quit</code>

## References
<ul>
    <li><a href="https://www.linkedin.com/learning/learning-haskell-programming/the-course-overview?resume=false&u=2195556">LinkedIn Learning</a></li>
</ul>

<a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/blog2.md">Next Post</a>
<a href="https://github.com/GaryZ700/Haskell_Blog/blob/master/README.md">Previous Post</a>
