# LearnR Lesson 2

# Basic R concepts ####################################################################


# Before we get going, let's do a little recap. We talked in the last lesson about packages and function:
#   - A package is a collection of functions, neatly 'packaged' into a downloadable, installable format. 
#   - Functions are the building blocks of R, and can be assembled into packages. They are like mini-programs that take inputs (called arguments), do something with them, and return an output or perform an action. For example, the print() function takes a string as input and prints it to the console. The syntax for using a function is: functionName(argument1, argument2, ...). For example, print("Hello world!") calls the print() function with "Hello world!" as the argument.

# To call a function, we can either do a library() and tell R that all function names in that package can be searched for, or we can use the packageName::functionName() syntax to be explicit about where the function comes from.



# Now instead of re-writing the wheel for how to learn R, I'll refer to the best resource I've found: the **swirl** package. Swirl is an interactive R package that teaches you the basics of R and more, right from the console (that's the window below this one). 



# To install swirl, run the following line of code:

install.packages("swirl") # This line installs the swirl package from CRAN (the Comprehensive R Archive Network)

# Once swirl is installed, you can load it into your R session in one of two ways:
#     swirl::swirl() # The notation here is packageName::functionName(), which is nice and explicit about where a function comes from.

#     library(swirl) # This loads the package into your R session, and you can then use any function from that package without the packageName:: prefix.
#     swirl() # This runs the swirl() function from the package you just loaded.

# Either of these options will start the swirl package and take you through a series of interactive lessons. Now, take whichever option you prefer and run the relevant code in the console below to start swirl.

# ..... 
# Go on now, run the code and get started with swirl. Use the hint below if you're unsure how to do this.
# .....
learnR_hint(lesson = 2, part = 1)



# After reading and action on a few prompts, you should get to a point where you can select one of five lessons. Start with less 1, 'The basics of programming in R'.

# You should push through the swirl lessons until you can confidently answer the following questions:
#     - What's the difference between R and RStudio?
#        - Can you run R without RStudio, and RStudio without R?
#     - What is a vector?
#        - What's the difference between a character and a numeric vector?
#     - What is a list?
#     - What is a data frame?
#     - How do you assign an object or value to a variable?
#     - How do you access a variable's value?

# Once you can check these items off your list, move on to the next section where you'll get thrown off the deep end. Take as long as you need!

# When you're ready to move on to the next lesson, run the line of code below:
learnR(lesson = 3)



