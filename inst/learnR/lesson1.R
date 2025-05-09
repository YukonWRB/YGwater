# LearnR Lesson 1

# Introduction and initial setup #####################################################


# This .R file is designed to help you learn the basics of R in an interactive manner.
# It's assumed that you are using a recent version of R (> version 4.1) and RStudio (> version 1.4). You should also be able to use this training using VSCode or another IDE, but the instructions are tailored for RStudio.
# We also assume that you are either already familiar with RStudio's default layout. At minimum, you should be able to identify the console, the script editor, and the environment pane. If not, please ask a colleague to help you out. You can also find a good introduction to RStudio here: https://posit.co/resources/rstudio-ide-overview/


# Have you ever opened a document in Notepad or R and noticed insanely long lines that force you to scroll laterally? That's annoying, right?
# Let's go ahead and modify some RStudio settings to make things easier.
# We'll make the lines in R scripts automatically flow based on your screen width.
#     In the top menu bar of RStudio, Go to Tools > Global Options. In the pop-up menu, select Code > Editing and check the box next to "Soft wrap source files".
#     While you're in there, take a moment ot explore the other options and modify then to your liking.
# You'll know you got the settings right if you don't have to scroll to see the entire line below (Line 17)!


# Now that you have RStudio set up, let's get started with the basics of this file you're reading. You'll likely have noticed that every line thus far starts with the '#' symbol; this indicates a comment in R, and comments are not run as code. On the other hand, lines that do not start with '#' are run as code. You can run a line of code by placing your cursor on the line and pressing Ctrl + Enter (or Cmd + Enter on Mac). Try running the line below:

print("Hello world!") # This line prints "Hello world!" to the console, below this window.

# You might have noticed that the line above used a comment **after** the print() statement; this is a common practice in R to explain what the code does, and everything after the '#' is ignored by R.


# Now let's touch on two important concepts: packages and functions.
#   - A package is a collection of functions, neatly 'packaged' into a downloadable, installable format. 
#   - Functions are the building blocks of R, and can be assembled into packages. They are like mini-programs that take inputs (called arguments), do something with them, and return an output or perform an action. For example, the print() function takes a string as input and prints it to the console. The syntax for using a function is: functionName(argument1, argument2, ...). For example, print("Hello world!") calls the print() function with "Hello world!" as the argument.

# A package can only ever have one function with a certain name, but the same function name could be found in multiple packages. Because of this, we need to tell R which package to look in when we call a function. Often, this is done with a library call:
# library(packageName)

# This tells R to load the package's function names into memory. If there's a conflict with an already loaded package, R will tell you and use the last loaded package's function.

# Another way to do things is to tell R which package to look in for a function directly:
# packageName::functionName(argument1, argument2, ...)
# This later method is more verbose but makes things crystal clear and removes any possibility for conflicts. However, library() calls are common since they make the code cleaner and easier to read. When you're starting out I **highly** recommend using fully qualified calls of packageName::functionName, as you'll get a much better understanding of what's going on.

# There's one big exception to that: base R functions. You **could*** use base::print("Hello world!") but that's unnecessary. Base R functions are always available, and you don't need to load them with a library call.



## Exercise 1 ##########################################################

# Now it's your turn to try it out. In the console below, make a library call to attach the YGwater package. Ask for a hint if you need one!
learnR_hint(exercise = 1)

# Still stuck? Call this to give you the answer:
learnR_answer(exercise = 1)


# That's it for this lesson! Now, run the line of code below to move on to the next lesson. R will know how to find that function since you're already done a library call.

learnR(lesson = 2)
