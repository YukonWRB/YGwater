# LearnR Lesson 3

# Working with data in R (post swirl lessons) ###########################################################



# Now that you know a thing or two about R, let's get into the nitty gritty of accessing the WRB's data directly and doing some fun stuff. You're surely aware of our main database by now, AquaCache; if not, talk to the Data Scientist and he'll explain the basics. We'll also practice data analysis from Excel files, since that's a common format for data storage and sharing, but before we even get to that we'll build our own dummy data set to play with.

# Before we get going though, an inportant note about how R is taught in web tutorials vs here:
# If you've poked around the web for R tutorials, you've probably come across the 'tidyverse' family of packages, including popular ones like dplyr, ggplot2, and tidyr. In theory, these packages are designed to make data manipulation and visualization easier and more intuitive; in practice however, they obscure the underlying data structure and make it harder to understand what's going on. You might have noticed that swirl doesn't use any tidyverse packages, and for good reason given the basic learning focus. We'll be using base R functions for most of our data manipulation and visualization tasks to keep this up. Note that it isn't **wrong** to use tidyverse packages, it's just much better in the long run to gain a solid base in basic R first.



# To start, let's create a dummy data set to play with. We'll use the 'data.frame()' function to create a data frame, which is a table-like structure that can hold different types of data in each column (you should remember this from swirl). The syntax for creating a data frame is:

#     data.frame(column1 = c(value1, value2, ...), 
#                column2 = c(value1, value2, ...), 
#                column3 = ...)

# Now go ahead and create a data.frame called 'df' with two columns: date (of class 'Date') and value (of class 'numeric'). Your dates should start in 2023-01-01 and end in 2023-12-31, and the values should follow a sine wave pattern with a period of 365 days. Refer to the help files for functions 'seq', 'seq.Date', and 'sin' to help you with this. This isn't easy, but give it a good shot

# Make the data.frame in the console, below this window, and name it 'df'

# If you need a hint, call up the function below:
learnR_hint(lesson = 3, part = 1)  # We're on lesson 3, get it??

# If you still need help and want the answer, call up the function below:
learnR_answer(lesson = 3, part = 1) # This is the answer to lesson 3

# Now let's get some basic statistics from this data. What's the mean, min, max of the 'value' column?
mean(df$value)
min(df$value)
max(df$value)
# You've probably figured this out, but df$value is a vector of values. Call it up in the console if you want to see what that looks like! We can't find the mean/min/max of an entire data.frame, only a vector.

# What about the mean, min, max of the values in the first 30 days? Remember, df$value is a vector, so we can use the square brackets to subset it. The syntax is:
#     df$value[1:30]
# Find the mean in the console, then compare your answer. You'll have to have already assigned 'df' to your environment though... if it's not there go back up a few lines in this script and do that.
learnR_answer(lesson = 3, part = 2)


# Great, now let's plot this data using R's base plotting method: the plot() function. The syntax is:
#     plot(x, y, type = "l", col = "blue", xlab = "x-axis label", ylab = "y-axis label")
# See the file file here:
?plot
# Now your turn: make us a plot! You should see a nice sine wave pattern in the plot window. If you don't, try again!
# If you're stuck, call up the function below:
learnR_answer(lesson = 3, part = 3)

# Now let's do a few more thing to practice manipulating data: let's modify the data so that our sine wave is amplified by a factor of 10. Remember that df$value is a vector? We can multiply it by 10 to amplify it (we can't multiply the whole data frame). Try it now, but remember that you have to assign the new values to the data.frame again.
learnR_answer(lesson = 3, part = 4)

# Now plot it again, exactly as before. You don't have to re-write the plotting code you used a few minutes ago though! With your cursor on the R console below, just use the 'up' arrow until you get to the plot command you used before, then hit enter. This is a great way to save time when you're working with R. Try it now!
# If you want to see the code you used before, call up the answer function from before:
learnR_answer(lesson = 3, part = 3)

# Lastly, let's get creative and make our sine wave plot cover two years. Now you can see howt the sine wave pattern goes from 0 at 2023-01-01 and finishes at 0 at the end of the year. Could we not just copy that again, changing the date? You bet!
# Let's first make a copy of the data.frame...
#   ... then tweak the dates...
#        ... then bring it all together.

# To copy the data.frame, make a new variable called 'df2' and assign to it the old one (df). Use a hint if you need.
learnR_hint(lesson = 3, part = 5)
# learnR_answer(lesson = 3, part = 5) # Uncomment this line to run it, or paste it in the console.

# Now let's modify the dates in the new data.frame. Two options here: add 365 days to each date, or make and assign a new sequence to the existing column. Option 1 seems easier, doesn't it? Try it now.
# learnR_answer(lesson = 3, part = 6)

# Cool, now you have a new data.frame with fast-forwarded dates! Take a peak at the top and bottom of it. Do you remember the head() and tail() functions? If not, check the help files for them.

# Did you notice anything about the tail of df2? December doesn't go all the way to then end of the month - it's a leap year! We'll ignore this for now (otherwise we'd have to calculate and add a corresponding value), but just note that using head() and tail() can give you really quick but useful glances at your data.

# Now let's add the two data.frames together and see what we get. Check out the help file for the rbind() function: do ?rbind
# If you get more than one option, selecte the one in package 'base'! This is a good example of why it's useful to know the package name of the function you're using.
# Now go on and rbind() the two data.frames together, into a new data.frame called 'df3'
# learnR_answer(lesson = 3, part = 7)
# Now let's plot this new data.frame. You can use the same code as before, but you'll have to change the name of the data.frame you're plotting from 'df' to 'df3'. Go ahead and do that now (remember to use the 'up' arrows from the console to get back to your plot command quickly).


# Phew, that was a fair bit of work, no? Let's end this lesson here and move on to the next one by runnning the line below.
learnR(lesson = 4)


