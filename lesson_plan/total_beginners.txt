Step 0: Gist of programming
---
Basic explanations about how code works
You write code in files
You then either
 - Use a program called a compiler to create a program from your code
 - Use a program called an interpreter to run the code you've written
Waving hands a little, all code falls into one of these two buckets
We'll be doing the second, using a program called Node.

Step 1: Basic tools (15 minutes)
---
Shell: We'll try to interact with this as little as possible.
 - A text based system of commands.
 - A really low-level way of talking to your operating system
 - Lots of programs talk to the operating system through this interface
 - Shell has a "current directory" that you're in.
 - You run commands with [command] [arg1] [arg2]
 - For example the command to change the directory would be to do "cd some_folder"
 - There are a lot of commands, but the basic ones we need are:
    cd: change directory
    ls: list directory
    node somefile: run somefile. Your computer probably doesn't have this one yet, but we'll get it.
    ...and anything else more complicated we'll leave out for now

Text editor:
- You can use any text editor you want, but not somethign that handles rich text.
- notepad would be perfect
- If you don't know one, we'll download VSCode. It's super easy and will be quick and easy

Node:
- Google node and download/install it.

Step 2: Hello World
---
In a folder you create using `mkdir folder`, create a file called hello.js

We'll get to what these things are in a second, but to make sure we've got everything correct,
we'll output "hello world"

```
    console.log("hello world");
```

This is telling node to run the javascript file hello.js, in which we've conveniently
written a "function call"
`node hello.js`

Let's dissect what we're looking at.
```
    console.log(             )
                ^ we're running a function (in this case the console.log function)

                "hello world" 
                 ^ we're passing the VALUE "hello world"
```


A value is something that is a specific piece of data.
The quotes mean "interpret this as text data". 

Here are some examples of values in javascript

```
    "i am value"
    1.05
    true
```

And as expected, we can pass these into the console.log function:

```
    console.log("i am a value");
    console.log(1.05);
    console.log(true);
```

Step 3: Comments
---
We can write informative text about a certian part of our program in the source
by using comments! In javascript, the way to write comments is using a //
Javascript ignores everything after the // as if it weren't there in the code at all!
For example:

```
    console.log('hello') // This text is completely ignored!
```

I'll use this when talking about specific parts in the code.

Step 4: Variables
---
If we want to store values so we can use it later, we'd do the same thing
we'd do in math and use a VARIABLE. This is how we do it in javascript, using
the special keyword var;

```
    var bestAnimal; // Declares a variable
    bestAnimal = giraffe;
```

In programming we generally instantly put a value into the variable right when we make the variable,
so javascript gives us a way to both declare and update a variable at the same time.
    var indespensableWisdom = "if your ball doesn't fit in your mouth, it isn't yours";

And exacly as you'd expect, we can now use that variable in other places!
    console.log(indespensableWisdom);

We can assign variables to other variables, just like we did when we assigned it to a value!
```
    var bestOfTimes = "These times";
    var worstOfTimes = bestOfTimes;
    console.log(worstOfTimes); // outputs "These times";
```

Step 5: Numbers
---
We can write down numbers!!
These are all examples of numbers
```
    1.0
    5
    15.2
    -100
```

We can add numbers
```
    1 + 1 // gives us 2
```

We can subtract numbers
```
    2 - 3 // gives us -1
```

And that's all we care about for numbers right now.

Step 6: Booleans
---
Right now, our program can't do very much "thinking". So we're going to introduce
some neater stuff. To start us off on this journey, we're going to talk about a new 
type of value (or data): The BOOLEAN (ooooo)

It's actually very simple. boolean is just a true or false value. 
Unsurprisingly, there are two of them: 
    true
    false

We can turn true into false and false into true by putting an ! in front of it!
    console.log(!false) // outputs true
    console.log(!true) // outputs false

! is just a mini-function so we can also do it to variables
    var first = true;
    var second = !first;
    console.log(second); // outputs false

Step 7: Comparing values
---
We care about booleans because we get them as the results of comparing numbers!

    1 == 1 // One equals one. Gives us true
    1 == 2 // One equals two. Gives us false
    1 != 2 // One does NOT equal one. Gives us true
    2 > 1  // Two is greater than one. 

You can try logging these functions!!

Step 8: If Statements
---
Now that we've got a boolean, we can introduce another thing, the IF STATEMENT
it looks a lot like the function call we did earlier, except directly after, it
has these weird curly braces. It tells us whether or not to do something:

```
    if (true) { }
```

We can put statements in between the curly braces:

```
    if (true) { console.log("Woo inside an if statement"); }
```

This looks kind of dumb, so we generally format these things like this:
```
    if (true) {
        console.log("Woo inside an if statement");
    }
```

(People will argue how to format this, but it really doesn't matter)
The idea is that if you pass in true, the statement(s) will run, and if you
pass in false, they won't!

```
    if (true) {
        console.log("I will run!");
    }

    if (false) {
        console.log("I won't run!!")
    }
```

like we could before, we can use variables!
```
    var shouldItRun = true; // initialize the variable
    if (shouldItRun) {
        console.log("This runs!");
    }

    shouldItRun = false; // update the variable with a new value
    if (shouldItRun) {
        console.log("This definitely does not run!");
    }
```

Step: Else clauses
---
If the condition doesn't work, we can create an ELSE clause that will run!
Its syntax is the following:

```
    if (shouldItRun) {
        // something that will happen if shouldItRun is true
    } else {
        // something that should happen if shouldItRun is false
    }
```

In a dead simple example:

```
    if (iAmEvil) {
        console.log("oh no I'm evil!!");
    } else {
        console.log("oh phew I'm good");
    }
```

Step 7: Looping
---
Looping is running over the same bit of code over and over.

There are a number of ways to loop, but we'll focus on the simplest
one to understand: the WHILE loop!

It looks very similar to the IF statement in syntax:
    while (someCondition) {}

It will repeatedly run as long as someCondition is true! Now we're getting somewhere!

    // Guess what this one does?? Can you tell me why it does that?
    while (true) {
        console.log("All work and no play makes Jack a dull boy");
    }

How about this?
```
    var i = 1;
    while (true) {
        console.log(i);
    }
```
Let's try something a little more nuanced
```
    while(false){

    }
```

So for example, we can make a "Count to 10" program
```
    var currentNumber = 1; 
    while(currentNumber < 11) {
        console.log(i); // 
        currentNumber = currentNumber + 1; // Add 1 to i
    }
```

See if you can run through the steps yourself of what it does.

Step 8: isDivisibleBy
---
We haven't shown how to make functions yet, and that's
probably for another time, because it doesn't really help us much
on our goal to get a working "fizzbuzz" type program.

And if you're interested, and/or we have time at the end, I'll go into it.
Just put this at the very top of your file and don't think too much about it.

```
    function isDivisibleBy(a, b){
        return b % a == 0;
    }
```

We can now use the isDivisibleBy function in the rest of our code!
```
    isDivisibleBy(10, 5); // 10 is divisible by 5, so this returns "true"
                 ^ When calling a function with multiple arguments, use a comma.
```

A function "returning a value" means that when we put it inline, that value gets 
used, e.g:

```
    var bestVar = isDivisibleBy(10, 5); // Assigns true to bestVar

    if (isDivisibleBy(a, b)) {
        console.log('a divides b')
    }
```

What does our trusty function console.log return? It's not very useful
```
    console.log(console.log('cat')) // outputs cat, then "undefined"
```

Step 9: Putting it all together
---
We'll be writing the infamous Fizzbuzz challenge:

Write a program that prints the numbers from 1 to 100. But for multiples of three print "Fizz" 
instead of the number and for the multiples of five print "Buzz". For numbers which are multiples 
of both three and five print "FizzBuzz".


Before attempting to tackle this, let's remind ourselves of the basic building blocks we have:
- Strings, numbers and bools: "hello", 145, true
- The ability create and assign variables.
- Some basic ways to change values: ! for bools, + and - for numbers
- Ways to compare values: ==, <, >
- Ways to conditionally run code
- Ways to loop over things
- and a function to say whether a certain number is divisible by another


Let's see if we can build towards this iteratively:
- Try writing a program that outputs the numbers 1 - 100, then STOPS
- Try writing a program that outputs


What did I miss?
---
We've barely scratched the surface of what there is to learn about writing
javascript. This was mostly meant as an intro of tho the PROCESS more than 
the language, but it's a very solid stepping stone on our way to understanding
more.

To demonstrate how muc there is to learn: 
Important beginner javascript topics we missed
- Defining functions
- Objects 
- Arrays
- For loops
- Scope / closures
- undefined
- Expressions vs Statements
- Why === is better than ==
- Modern features such as let, const, arrow notation, etc.
