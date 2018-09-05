# POLITSC 7551, Fall 2018
# Instructor: Jan Pierskalla
# Thanks to Jacob Montgomery for sharing his original R scripts!
# Modified by Adam Lauretig, AU 2018
#################################################
# Lab 1 Basics: What the ...?
#################################################

#1.1: Install R and RStudio
# http://cran.r-project.org/bin/
# http://www.rstudio.com/
# works on all platforms
# Unless you are compiling your own software, you will always want 'binary' distributions of software.


#1.2 Basics
# It is possible to do some things using the menus, but we will mostly use command line entry.
# We will also create scripts (such as this one) that can be run all at once, or using shortcut keys from the script editor.

# In Windows/Mac:
#1) Start RStudio
#2) File --> New File --> R Script
#3) Write the command

print("Hello world")

#4) Highlight the line and hit Ctrl-enter/command-enter


# A few features of R:
# Everything in R is object oriented. Data, functions, inputs and outputs will all be data objects.
# R is case sensitive. For that matter, it is also spelling sensitive.
# R is an open-source platform that comes with many basic functions.
# Additional functionality are available in user-contributed packages that can be downloaded, loaded, and used.
# Lots and lots of people use R, write handouts for class, write websites, etc.  Google is your friend (and your enemy).

# 1.3: R as a calculator

# R can handle any basic arithmetic problem
5+4 # Addition
6-3 # Subtraction
34 / 6 # Division
5 * 3 # Multiplication
5^4 # Exponents
625^(1/4) # More exponents

# R comes with a number of constants pre-stored that you can use
6.25 # numbers
pi # And a few others
NA # Missing value
NULL # Nothing.
0/0 # NaN means "Not a number"
1/0 # Inf means infinity

# Please Excuse My Dear Aunt Sally
2*(3-4)+2
2*(3-4)+2*(4 + 3)^(1/3)

# Use the up and down arrows to access previously typed commands


#1.4 Installing packages
# The beauty of R is that there are packages, although things can be a bit unorganized.

install.packages("BAS") # This will prompt a user interface to choose the "mirror" or repository
library(BAS) # this will actually load the library for use.  You must call this every time.
search() # you can see what packages are attached to the workspace (and also what other objects)
help(package="BAS") # Will (usually) give you a list of functions for the package
example(BAS) # some package writers give you little examples to get you started
## All packages documentation are on CRAN

## Many packages will have associated publications in the Journal of Statistical Software

# TIP: install the package "swirl", it contains a set of self-guided tutorials that will teach you R


#1.5: Objects and named storage

# What if we wanted to calculate the % of eligible voters who turnout in the 06 elections in Alaska
# Data source: http://elections.gmu.edu/Turnout_2006G.html


turnout_ak <- 238307/496387 # Turnout divided by voting age population
# the <- operator asigns a value to that object
turnout_ak # the command 'print(object)' will be called if you just type the name
turnout_ak = 238307/496387 # You can also use an equal sign .. but shouldn't


# Named storage also lets you handle each piece separately
total_votes_ak <-  238307
voting_age_population_ak <- 496387
turnout_ak <- total_votes_ak/voting_age_population_ak

# Object names cannot start with numbers
06election <- 50000 ## no

# Named objects are stored in the "global environment", which means that they can be accessed at any time by any function you might run.
# They are "global" variables (which makes them different from "local" variables).

objects() # List the objects currently on your global
ls() # same thing

# you can also remove objects
Jan <- 100
ls()
rm(Jan)
# The command remove(Jan) does the same thing
ls()
Jan1 <- Jan2 <- 100
ls()
rm(list = c("Jan1", "Jan2"))
objects()

# combine them to remove everything
rm(list = ls()) # useful to put at the beginning of your script


#1.6  Basic inputs and outputs

# say we have made some R object we want to save
vap <- voting_age_population <- c(3481823, 496387, 4582842, 2120139,26955438,3617942,2673154,652189,472143,14085749,6915512,995937,1073799,9600372,4732010,2265860,2068253,3213141,3188765,1033632,4242214,4997677,7620982,3908159,2139918,4426278,731365,1321923,1870315,1012033,6598368,1452962,14838076,6752018,494923,8697456,2697855,2850525,9612380,824854,3303593,594599,4636679,17038979,1797941,487900,5841335,4876661,1421717,4257230,392344)
# voting age population of the 50 states plus DC
# Notice the double asignment. I've created two identical objects

total_votes <- tv <- c(NA, 238307, 1553032, 780409,8899059,1586105, 1162391,258053, 122356,4884544, 2143845,348988, 458927,3586292, 1719351,1071509, 864083,1370062, 954896,NA, 1809237, 2243835,3852008, 2217552,NA, 2178278, 411061,610499, 586274,418550, 2315643,568597, 4703830,2036451, 220479,4184072, NA,1399650, NA,392882, 1117311,341105, 1868363,NA, 582561, 263025,2398589, 2085074,473014, 2183155, 196217)
voting_data <- data.frame(vap, tv) # making a data frame of our vectors ___ more on this next week

# First we want to change the working directory.  This is the folder where R will save and look for data by default (although you can always override this)
setwd("~/Dropbox/OSUClasses/MethodsUG/Data/") # Mac
setwd("F:/MethodsUG/") # Windows 
# you have to pick the correct directory, e.g., the folder with all your data files.
# You can always check the directory path of folders and files by right-clicking on the file and 
# checking the "Get Info" or "Properties" tab

# You can also hard-code file paths, which makes collaborating much easier
# rather than setting the working directory, you write out the entire "location"
# the object you want to save/load

# TIP: Copy and paste.

#Now let's save our newly created dataset
save(voting_data,file = "~/Desktop/voting_data.Rdata")

# Now we can clear our workspace, and load the file again
rm(list=ls())
load("~/Desktop/voting_data.Rdata")
ls() # only saved objects present

# some Freedom House data saved in Stata's .dta format
library(foreign)

freedom <- read.dta("fh.dta") # Mac - can also give entire filepath
freedom <- read.dta("F:/Quant_I/fh.dta") # Windows
freedom

# this data frame has rows for each observation and columns for each variable
# you can access specific rows like this:
freedom[5, ]

# you can access specific columns like this:
freedom[ ,2] # note that elements of one-dimensional objects, like vectors, are accessed with x[], but with the two-dimensions of a data.frame, you need to specify rows and columns x[row,column]
freedom[ ,"fh_pr"]
freedom$fh_pr # this is how we typically extract specific variables

# you can also save any data.frame as a .dta file (Stata's file format)
write.dta(freedom, file = "freedom.dta", version = 10)

# load the some data in .csv format
fh <- read.csv("~/Dropbox/Data/fh.csv") # Mac
fh <- read.csv("F:/Quant_I/fh.csv") # Windows

fh

write.csv(freedom, file = "freedom.csv")


#1.7: Introduction to functions

# Functions are objects that contain a list of instructions
# We "call" a function to execute a command

log # log is a function that takes two commands, an input of data and instructions on the correct base
# the second argument has the default argument exp(1).
?log # the  "?" calls the help file for each function


exp(1) # exp(1) is itself a function that produces euler's constant

log(2) # ln(2)
log(2, base = 10) # log(2) base 10

# There are a *lot* of functions in R.  Finding the right ones is really almost half of the battle
apropos("norm") # finds all functions in your version of R with the string "norm" in the name

# you can call functions inside eachother.  The computer will evaluate the "inside" one first.
exp(log(1)) # e^ln(1) = 1

#1.8: Vectors

c(0, 7, 8) # the c() function is used to collect/concatenate things together into a vector
x <- c(0, 7, 8) # assign this to a named object
x # we can see x

numbers_5_to_20 <- 5:20 # the : symbol creates sequences of increasing or decreasing numbers
numbers_5_to_20


1.5:10 # always increments by 1, and may not get to top number

c(numbers_5_to_20, x) #concatenate two vectors together

vap <- voting_age_population <- c(3481823, 496387, 4582842, 2120139,26955438,3617942,2673154,652189,472143,14085749,6915512,995937,1073799,9600372,4732010,2265860,2068253,3213141,3188765,1033632,4242214,4997677,7620982,3908159,2139918,4426278,731365,1321923,1870315,1012033,6598368,1452962,14838076,6752018,494923,8697456,2697855,2850525,9612380,824854,3303593,594599,4636679,17038979,1797941,487900,5841335,4876661,1421717,4257230,392344)
# voting age population of the 50 states plus DC
# Notice the double assignment. I've created two identical objects

total_votes <- tv <- c(NA, 238307, 1553032, 780409,8899059,1586105, 1162391,258053, 122356,4884544, 2143845,348988, 458927,3586292, 1719351,1071509, 864083,1370062, 954896,NA, 1809237, 2243835,3852008, 2217552,NA, 2178278, 411061,610499, 586274,418550, 2315643,568597, 4703830,2036451, 220479,4184072, NA,1399650, NA,392882, 1117311,341105, 1868363,NA, 582561, 263025,2398589, 2085074,473014, 2183155, 196217)

# Accessing elements of a vector or matrix is usually done with the [] operators
vap[1] # accessing the first element of the vector.  
vap[40] # printing the 40th element
vap[c(3, 6, 7)] # We can extract several elements at a time
numbers_5_to_20[3:7] # we just need to feed a vector of values into the indexing brackets
total_votes[c(3, 3, 2, 2)] # We can repeat numbers as well
x[-1] # printing everything but the first element
vap[-(3:51)] # print everything but the 3rd through 51st elements
vap[0] # the 0th index returns nothing
vap[c(0:4)]
x[c(-2, 3)] # do not mix positive and negative indexes

a <- c(1,2)
a <- c(a, 3) # can do this recursively

# Basic vector operations
x
x * 3 #scalar multiplication
y <- x-5 #simple addition and multiplication are done "by element"
y
x^3 # ditto with exponents
y^x # but if each are three elements long, it will execute by element

# When vectors have different lengths, the shorter one is extended by repeating the vector
# This means two things: 1) The vector lengths must be multiples of each other
# 2) This is a *very* easy way to make a bad, bad mistake.

tv[2:3]/vap[2:5] # this is crazy land.  Dividing by the wrong elements
tv[2:5]/vap[2:5] # this is correct

# For these kinds of reasons, it's nice to be able to access the length of a vector
length(tv)
length(vap)
length(tv[3:20])

# A lot (most?) functions in R are designed to work very cleanly with vectors.
# Many of them work on each element seperately
sqrt(vap) # square root of ever element in the vector
log(vap) #  Natural log of each element
abs(c(-1,2,-1)) # absolute value

# Others will use the whole vector as its input
mean(vap) # the mean voting age population
var(vap)  # sample variance of the vap
sd(vap)  # sample standard deviation
max(tv, na.rm = T) # maximum -- ignore missing data
min(tv, na.rm = TRUE) #minimum -- notice that T=TRUE
summary(vap) #
sort(tv) # creates a sorted vector
sum(vap) # Adds the vector together
prod(vap) # multiplies the vector --
cumsum(vap) # Cumulative sum
cumprod(vap) # cumulative product

# Others will take in the vectors, and return a different vector of the same length
is.na(tv) # Returns a vector of "TRUE" or "FALSE" based on whether or not the item is missing (NA)
!is.na(tv) # Returns the opposite of the above.  The "!" symbol means not.

# Class exercise: What's going on here?
mean(tv[!is.na(tv)]/vap[!is.na(tv)])
mean(tv/vap)
mean(tv/vap,na.rm = TRUE) # in  many functions the "na.rm=T" option tells the function to ignore missing data


# There are other functions that will make vectors as outputs
rep(1, 5) # repeat the value 1, 5 times
seq(1, 21, by = 2) # Make the sequence 1 to 21 moving by increments of 2
rep(seq(2,20, by = 2), 2) # repeat the pattern 2, 4, ... 20, twice
rep(c(1,4), c(3,2)) # repeat 1, 3 times and 4, twice
rep(c(1,4), each = 3) # repeat each value 3 times

# what's going on here?
rep(seq(2,20,2), rep(2,10))

#1.9 Types of objects: Classes

# We have already come across two types ("classes") of objects
class(log) # functions
class(total_votes) # numeric

# There are lots of "types" of objects.  These are just properties of the object that come with instructions telling R how to store the data, and (for some functions) what to do when certain functions are called on them.  For instance, you cannot take the mean of a text object, so it will throw an error.

# But there are two more  data types that you will use at any time
class(TRUE) # logical (often called Boolean)
class("Now is the winter of our discontent") # character (often called string)

# Sometimes there are functions to move back and forth between types
#as.integer(grp) # There are lots of "as.class()" functions you can look up
#is.integer(x) # can also test if an object meets specific standards
#is.numeric(x)

# Vectors can only be of one type
xx <- c(0, 2, "Monkey", 5) # Numbers get changed to strings
xx
class(xx)
xx <- c(TRUE, FALSE,0, TRUE) # Logicals get recast as numerics
xx
class(xx)

# Useful trick:
c(TRUE, FALSE, TRUE, FALSE) * 1



# 1.10 More on Boolean Logic
## use logicals to subset, recode, or otherwise handle data

x <- c("Tunisia","USA","Germany")
chooser <- c(T, F, T)
x[chooser] # print ony those elements of x where chooser is TRUE
sum(chooser) # Arithmetic on logicals creates numerics
!chooser # not operator


# We can generate logical vectors using basic boolean algebra
x <- 2
x == 7
x >7
x>=7
x<7
x!=7
x<=7
x<7 | x ==7 ## the or operator
x<=7 & x == 7 ## the and operator

# There are also some functions that create logicals.
is.na(total_votes) # vectorized
is.numeric(total_votes) # this is not vectorized

# You can use these to do fun things to data

# subset
small_states <- voting_age_population[voting_age_population < median(voting_age_population)]
small_states

# recode
state_size <- (voting_age_population < median(voting_age_population)) *1
state_size

state_size <- ifelse(voting_age_population < median(voting_age_population),1,0)
state_size

# or this
state_size[state_size == 1] <- 3
state_size
# or this
state_size[state_size > 0] <- 5
state_size
# or this
state_size <- ifelse(state_size==5,1,0)
state_size

# Tip: Be *VERY* careful about combining & with | -- especially in the presence of !


#1.11 Getting help

# Learning about functions and how to specify them correctly is half the battle
help(sqrt) # help w/ functions
?sqrt # same thing
help.start() # lots of help
help.search("sqrt") # what am I looking for? Fuzzy matching
example(sqrt)

## TIPS:
# 1) Remember that these help menus are usually written by the same people who wrote the functions you are using.  They are uniformly not helpful unless you already know a good bit about computer programming and (in some cases) a lot about the function itself.
# 2)  There is a basic structure that all help files must meet, and it is *very* important that you try and get the hang of this.  Let's go through this help menu together:
help(mean)
# 3) You can try and type the name of the function to see the code, although that is not always helpful.


# 1.12 a review of sampling

# let's assume for a moment that our population of interest are the 403 respondents in the Cooperative Congressional Election Study
# We are interested in the average favorability ranking of John Stewart.
# Here we have data for the whole population. What could we learn though from a sample?

# load the data

cces <- read.csv("~/Dropbox/OSUClasses/Quant_I/Data/cces.csv") # Mac
cces <- read.csv("F:/Quant_I/cces.csv") # Windows


# "true" favorability of John Stewart in the population:

truth <- mean(cces$stewart)
truth

# We could start with a single datum, let's pick one respondent out of the population:

datum <- cces[100,]
datum[,4]

# that is much higher than the "truth", not surprising though -> random deviation due to sampling

# let's see if we can do better by drawing a larger sample. The sample() function will be our friend
# First we determine the rows we want to sample
subsample <- sample(1:length(cces[,1]), size=25, replace=FALSE)
subsample
# Now, we pick those rows form the population
cces_sample1 <- cces[subsample,]
length(cces_sample1$stewart) #check if it is equal to 25

# let's calculate the mean
mean(cces_sample1$stewart)

# ok, let's increase the sample size
subsample <- sample(1:length(cces[,1]), size=50, replace=FALSE)
cces_sample2 <- cces[subsample,]

subsample <- sample(1:length(cces[,1]), size=100, replace=FALSE)
cces_sample3 <- cces[subsample,]

subsample <- sample(1:length(cces[,1]), size=200, replace=FALSE)
cces_sample4 <- cces[subsample,]

# Now, we put all the means into one vector and the sample size in another
means <- c(datum[,4],mean(cces_sample1$stewart),mean(cces_sample2$stewart),mean(cces_sample3$stewart),mean(cces_sample4$stewart))
means
n <- c(1, 25, 50, 100, 200)
n

install.packages("ggplot2")
library(ggplot2)
qplot(n, means, geom="point", ymin=2)  + geom_hline(aes(yintercept=mean(truth)))


##############################################
# Bonus Material
##############################################

# Bonus I: Law of Large Numbers 

set.seed(1212) # sets the random number generator
n <- 50000  # number of coin flips we want to simulate
p <- 0.3 # probability of head
x <- sample(0:1, n, repl=T, prob=c(1-p, p)) # flip 50000 coins and store the result
s <- cumsum(x) # creates a vector that counts the running number of heads
r <- s/(1:n) # calculates the percentage of heads after trial i
lo <- max(c(0, p-0.1)) # sets limits for the y-axis in the plot
hi <- min(c(1, p+0.1)) 
plot(r, ylim=c(lo, hi), type="l") # plots the percentage of heads after trial i
lines(c(0,n), c(p,p)) # adds a horizontal line to the plot, indicating the true probability
r[n]



# Bonus II: Special character functions

# Some functions can only be used on specific data types.  This will be listed in the help files.

colors <- c("red", "yellow", "blue") # what happens if we don't use quotes?
more_colors <- c(colors, "green", "magenta", "cyan")

# you might want only a portion of the string
substr(colors, start=1, stop=2) # for ach string show the part of the string in the start position to the stop position
substr(colors, 1,2) # Remember -- you don't have to use the options, but it helps

paste(colors, "flowers") # combine two strings
paste("I like", colors, "flowers")
paste("I like", colors, "flowers", collapse = "") # The "" means the empty string.  Like NA for numeric

nchar(colors) # how many characters in each string

extreme_statement <- "POLITSC 7551 is my life"
this_out <- strsplit(extreme_statement, split=" ") # divide the string into seperate elements
unlist(this_out) # The outtput for strsplit is a list ... more on this later

# This is not a "special" string function, but I only use it with strings.  You can use it
# replace specific statements
gsub("my life", "the bee's knees", extreme_statement) # less extreme.  More true


# Tips:
# If you are doing a lot with strings try the library 'strgr', which has some user-friendly functions
# A very common programming error (at least for me) is to forget a closing quotation mark.  This will make the computer think you are still making a giantly long string.












