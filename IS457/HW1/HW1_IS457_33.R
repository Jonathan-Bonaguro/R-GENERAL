# Do not remove any of the comments. These are marked by #

# HW 1 - Due Monday Sep 17, 2018 in moodle at 1pmCT and hardcopy in class.

# (1). Please upload R code and report to Moodle 
#      with filename: HW1_IS457_YourClassID.
# (2). Turn in a hard copy of your report in class 
#      without your name but only your class ID, 
#      violators will be subject to a points deduction.

## Important: Make sure there is no identifying information on your printout, including name, username etc. 
## Only include your class ID on there. 

# Part 1. LifeCycleSavings Data

# In this part, we will work with a built-in dataset -- LifeCycleSavings.
# (1) R has a built-in help funtion, write your call to the help function below, as well as something
# that you learned about this dataset from the help function. (1 pt)

# Your code/answer here
help("LifeCycleSavings")
## I never knew about the life-cycle savings hypothesis, and that it is explained by disposable income and
## change in disposable income across demographic groups of age 0-15 and 75+. 

# (2) Describe this dataset (structure, variables, value types, size, etc.) (2 pts)

# Your code/answer here
help("LifeCycleSavings")
## This dataset consists of 5 vectors of length 50. These represent the 50 observations of the 5 variables. 
## All variables are of the numeric type. 

# (3) What is "aggregate personal savings" in this dataset? Calculate the average aggregate 
# personal savings of these 50 countries. (1 pt)

# Your code/answer here
## Aggregate personal savings are the variable "sr" in this dataset. 
mean(LifeCycleSavings$sr)

# (4) What is "dpi" in this dataset? Find the highest and lowest dpi. (2 pts)

# Your code/answer here
##"dpi" is the amount of real per-capita disposable income averaged over the decade 1960-1970. 
min(LifeCycleSavings$dpi)

max(LifeCycleSavings$dpi)


# (5) How many countries have a dpi above median? (2 pts)
# hint: you might need to find a function to count rows.

# Your code/answer here
median(LifeCycleSavings$dpi)

length(which(LifeCycleSavings$dpi>695.665))


# (6) What is the highest aggregate personal savings of the countries 
# whose pop15s are more than 10 times their pop75s? (2 pts)

# Your code/answer here
max(LifeCycleSavings$sr[LifeCycleSavings$pop15 > 10*LifeCycleSavings$pop75])

# (7) For the countries with dpi above the 75th percentile, what is their average aggregate personal savings? 
# For the countries with dpi above the 75th percentale, what is their median aggregate personal savings?
# Why are these two statistics different?

# Your code/answer here
mean(LifeCycleSavings$sr[LifeCycleSavings$dpi > quantile(LifeCycleSavings$dpi, .75)])

median(LifeCycleSavings$sr[LifeCycleSavings$dpi > quantile(LifeCycleSavings$dpi, .75)])


#These statistics are different because the average is calculated by taking the sum of the observations and 
#dividing them by the total number of ovservations, and the median is the value that lies in the middle of 
#the data set. 

# (8) Let's look at countries with dpi below the 25th percentile. What is their average and their median
# aggregate personal savings? 
# Why are these two statistics different? Is the pattern of difference different than what you saw in 
# Q7? Why or Why not?

# Your code/answer here
mean(LifeCycleSavings$sr[LifeCycleSavings$dpi < quantile(LifeCycleSavings$dpi, .25)])

median(LifeCycleSavings$sr[LifeCycleSavings$dpi < quantile(LifeCycleSavings$dpi, .25)])


#These statistics are different because the average is calculated by taking the sum of the observations and 
#dividing them by the total number of ovservations, and the median is the value that lies in the middle of 
#the data set. 
#The pattern of difference is different than Q7 in that the median is signficantly lower than the average.
#One possible explanation of this is that there are more observations of aggregate personal savings with
#lower values, but the observations with higher values are significantly higher observations. 

# (9). (3 pts)
# Try running each expression in R.
# Record the error message in a comment
# Explain what it means. 
# Be sure to directly relate the wording of the error message with 
# the problem you find in the expression.

#LifeCycleSavings[LifeCycleSavings$pop15 > 10]
### Error message here
#Error in `[.data.frame`(LifeCycleSavings, LifeCycleSavings$pop15 > 10) : 
#undefined columns selected
### Explanation here
#The brackets are trying to subset LifeCycleSavings and then looking for the data where it does not exist 
#after it was subset. If you remove the brackerst and the first "LifeCycleSavings", you can find out
#which rows have the variable pop15>10.

#mean(pop15,pop75)
### Error message here
#Error in mean(pop15, pop75) : object 'pop15' not found
### Explanation here
#This error code comes from there being no information on where to find the variables. This can be included
#by either using $ before each variable or including 'data ='.

#mean(LifeCycleSavings$pop15, LifeCycleSavings$pop75)
### Error message here
#Error in mean.default(LifeCycleSavings$pop15, LifeCycleSavings$pop75) : 
#'trim' must be numeric of length one
### Explanation here
#This error comes from trying to run two experessions in a function that can only return a "numeric of 
#length one" or one response. You can either find the mean of pop15 or pop75, but not both at once. 

# Part 2. Plot analysis

# Run the following code to make a plot.
# (don't worry right now about what this code is doing)
plot(LifeCycleSavings$pop15, LifeCycleSavings$pop75, xlab = 'pop15', ylab = 'pop75', main = 'pop15 vs pop75')

# (1) Use the Zoom button in the Plots window to enlarge the plot. 
# Resize the plot so that it is long and short, making it easier to read.
# Include an image of this plot in the homework you turn in. (1 pt)

# Your answer here


# (2) Make an interesting observation about the relationship between
#     pop15 and pop75 based on your plot. 
# (something that you couldn't see with the calculations so far.) (1 pt)

# Your answer here
#There seems to be a relationship between the percentage of the Population under the age of 15 and the amount
#of the population over the age of 75. The higher the percentage of people under the age of 15, the lower
#percentage of people over 75. 

# (3) Based on our analysis so far, what interesting question about the LifeCycleSavings data
# would you like to answer, but don't yet know how to do it? (1 pt)

# Your answer here
#I would like to see if there is a relationship between the amount of population over the age of 75
#and the amount of personal savings. Does more people over the age of 75 mean there are higher amounts
#of personal savings?

# Part 3. Random number generators
# For the remainder of this assignment we will work with 
# one of the random number generators in R.

# (1) Use you UIN number to set the seed in the set.seed() function. (1 pt)

# Your code here
set.seed(660694484)

# (2) Generate a vector called "normsample" containing 1000 random samples from a 
# normal distribution with mean 2 and standard deviation 1. (1 pt)

# Your code here
normsample <- rnorm(1000, mean=2, sd=1)

# (3) Calculate the mean and standard deviation of the normsample. (2 pts)

# Your code here
mean(normsample)
#2.016063
sd(normsample)
#1.002991

# (4) Use logical operations (>,<,==,....) to calculate
# the fraction of the values in "normsample" that are more than 3. (1 pt)

# Your code here
length(which(normsample>3))
168/1000

# (8). Find the area under the normal(2, 1) curve to the right of 3.  
# This should be the probability of getting a random value more than 3. 
# (Hint: Look up the help for rnorm. You will see a few other functions listed.  
#  Use one of them to figure out about what answer you should expect.)
# What value do you expect? 
# I would expect about 16.8%. 
# What value did you get?
# R returned the value 15.9%
# Why might they be different? (3 pts)
#The values may be different based on a few things. They may be that the random values will differ based
#on a vector of different lengths. In this case, this results in a lower probability of giving a value 
#more than three. Probabiliy does not neccesarily predict observed results. 

# Your code here
help(rnorm)
pnorm(3, mean=2, sd=1, lower.tail = FALSE)
