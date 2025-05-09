# LearnR Lesson 3

# Working with data in R (post swirl lessons) ###########################################################



# Now that you know a thing or two about R, let's get into the nitty gritty of accessing the WRB's data directly and doing some fun stuff. You're surely aware of our main database by now, AquaCache; if not, talk to the Data Scientist and he'll explain the basics. We'll also practice data analysis from Excel files, since that's a common format for data storage and sharing, but before we even get to that we'll build our own dummy data set to play with.

# Before we get going though, an inportant note about how R is taught in web tutorials vs here:
# If you've poked around the web for R tutorials, you've probably come across the 'tidyverse' family of packages, including popular ones like dplyr, ggplot2, and tidyr. In theory, these packages are designed to make data manipulation and visualization easier and more intuitive; in practice however, they obscure the underlying data structure and make it harder to understand what's going on. You might have noticed that swirl doesn't use any tidyverse packages, and for good reason given the basic learning focus. We'll be using base R functions for most of our data manipulation and visualization tasks to keep this up. Note that it isn't **wrong** to use tidyverse packages, it's just much better in the long run to gain a solid base in basic R first.



## Exercise 2 ##########################################################
# To start, let's create a dummy data set to play with. We'll use the 'data.frame()' function to create a data frame, which is a table-like structure that can hold different types of data in each column (you should remember this from swirl). The syntax for creating a data frame is:

#     data.frame(column1 = c(value1, value2, ...), 
#                column2 = c(value1, value2, ...), 
#                column3 = ...)

# Now go ahead and create a data.frame called 'df' with two columns: date (of class 'Date') and value (of class 'numeric'). Your dates should start in 2023-01-01 and end in 2023-12-31, and the values should follow a sine wave pattern with a period of 365 days. Refer to the help files for functions 'seq', 'seq.Date', and 'sin' to help you with this.
# Make the data.frame in the console, below this window, and name it 'df'
# If you need a hint, call up the function below:
learnR_hint(exercise = 2)  # We're on Exercise 2, get it??

# If you still need help and want the answer, call up the function below:
learnR_answer(exercise = 2)

# Now let's get some basic statistics from this data. What's the mean, min, max? You should know this if you remember high school math... guess!
mean(df$value)

# Great, now let's plot this data:
plot(df$date, df$value, type = "l", xlab = "Date", ylab = "Value", main = "Sine wave pattern")
# You should see a nice sine wave pattern in the plot window. If you don't, try again!




