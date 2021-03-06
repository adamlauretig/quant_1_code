
# # Matrix Algebra and Functions
# 
# There are five basic data structures in R: vectors, matrices, arrays, lists, and data.frames. We'll be going through each of these here, but if you want an in depth exploration of these I'd recommend Norman Matloff's *The Art of R Programming: A Tour of Statistical Software Design*.
# 
# ## Matrix basics
# 
# Up to this point, we've primarily *talked* about vectors. We've encountered other data types, but haven't used them. Vectors have length, but no width (they can only represent one variable at a time). Matrices are just collections of vectors (exactly like you learned in math camp). We can combine them by column using \texttt{cbind}, or by row, using \texttt{rbind}. We then access elements of matrix by \texttt{matrix[row, column]}.
# 
## ---- matrix1, echo = TRUE, cache = TRUE---------------------------------
vap <- voting.age.population <- c(3481823, 496387, 4582842, 2120139,26955438,3617942,2673154,652189,472143,14085749,6915512,995937,1073799,9600372,4732010,2265860,2068253,3213141,3188765,1033632,4242214,4997677,7620982,3908159,2139918,4426278,731365,1321923,1870315,1012033,6598368,1452962,14838076,6752018,494923,8697456,2697855,2850525,9612380,824854,3303593,594599,4636679,17038979,1797941,487900,5841335,4876661,1421717,4257230,392344)

total.votes <- tv <- c(NA, 238307, 1553032, 780409,8899059,1586105, 1162391,258053, 122356,4884544, 2143845,348988, 458927,3586292, 1719351,1071509, 864083,1370062, 954896,NA, 1809237, 2243835,3852008, 2217552,NA, 2178278, 411061,610499, 586274,418550, 2315643,568597, 4703830,2036451, 220479,4184072, NA,1399650, NA,392882, 1117311,341105, 1868363,NA, 582561, 263025,2398589, 2085074,473014, 2183155, 196217)

m1 <- cbind(vap, tv) # Combined by column
m2 <- rbind(vap, tv) # combined by row
m2[1,2] # first row, second column
m1[,1] # the ith colum
m1[1:5,1:2] # a submatrix
m2[1,1:10]
m2[1:2, 1:10]
m2[, 1:10] # same as previous line since there are only two rows.
class(m2)

# 
# However, we can also create matrices directly, we don't need to create vectors first:
# 
## ---- matrix2, echo = TRUE-----------------------------------------------

#Another way to specify a matrix
matrix(1:10, nrow = 5)
matrix(1:10, ncol = 2) #the same
matrix(1:10, nrow = 5, ncol = 2) # the same
matrix(1:10, nrow = 5, byrow = TRUE) ## not the same


# 
# By default, R will fill each column of a matrix, and then move to the next one. If you specify \texttt{byrow = TRUE}, however, R will fill each row, and then move onto the next one. 
# 
# \clearpage
# 
# ## Arrays and attributes
# 
# Arrays are a more general way to store data. Where a matrix can only have 2 dimensions (rows and columns), arrays can have an arbitrary number of dimensions, but this *will* increase the amount of memory they consume. 
# 
# Let's examine a cube of dimensions $3 \times 4 \times 2$. One way of thinking of this is two $3 \times 4$ matrices stacked on top of each other:
# 
## ---- array1, echo = TRUE------------------------------------------------
a <- array(1:24, dim = c(3, 4, 2))
a

# 
# Since this array has three dimensions, there are now three indices we can use to access the array:
# 
## ---- array2, echo = TRUE------------------------------------------------
a[, , 1]
a[, 1, ]
a[1, , ]
a[1, 1, ]
a[, 1, 1]
a[1, 1, 1]


# 
# 
# Notice that the 'dim' is asssigned.  This is an "attribute" of the array; attributes are some piece of data associated with the structure that isn't the data itself, and are used to make working with these data easier.
## ---- array3, echo = TRUE------------------------------------------------

dim(a) 
attributes(a)
str(a)


# 
# 
# 
# Matrices also have this attribute (\texttt{dim}), and also have and attribute \texttt{dimnames()}, which are strings (technically lists of strings, but we'll get to that in a minute), which allow you to label your observations.
#  
## ---- matrix3, echo = TRUE-----------------------------------------------

dim(m1) # number of rows, number of columns
attributes(m1) # there is another attribute here -- the columns have names
dimnames(m1) # we can either assign or get the dimnames attribute
# The first part is the rownames (which we didn't assign)
dimnames(m2) # here the columns have no names
dimnames(m1)[[2]][1]<-"Dracula"
head(m1) # We have re-named the first column to have the name "Dracula"
dimnames(m1)[[2]][1]<-"vap" # all of this bracketing is because this is a list ... what's a list?
head(m1)

# 
# 
# R is flexible, and there are multiple ways to access dimnames:
# 
## ---- matrix4------------------------------------------------------------

# Another way to do this
colnames(m1)
# How would we rename the first column?
colnames(m2)
rownames(m1)
rownames(m2)


# 
# \clearpage
# 
# ## Lists
# 
# One downside to matrices and vectors is that every element in them must be the same type (all numerics, or all intergers, or all character vectors).  Lists offer a way around this restriction, they can combine multiple data types. Lists are a very flexible way to store data, and are maybe the most common data structure you'll encounter: many functions produce lists.
# 
## ---- lists1, echo = TRUE, cache = TRUE----------------------------------

list.a <- list(m1, vap, 3) # m1 is a matrix, vap is a vector, 3 is an integer
list.a


# 
# 
# We can make all sorts of lists, and can even create lists containing other lists!
# 
## ---- lists2, echo = TRUE, cache = TRUE----------------------------------

vector1 <- c(1,2,3)
gospels <- c("matthew","mark","luke", "john")
my_matrix <- matrix(c(1:20), nrow=4)
my_data <- data.frame(cbind(vap, tv))
my_crazy.list <- list(vector1, gospels, my_matrix, TRUE, list.a)
my_crazy.list # we can combine anything we want -- we can even include other lists in our lists


# 
# What if we want to access the attributes of our list?
# 
## ---- lists3, echo = TRUE------------------------------------------------

str(my_crazy.list) # the str() function is useful for looking at the basic components 
# of any complicated object like this
#str() will work with most types of objects

attributes(my_crazy.list) # lists has attributes, but we haven't set them
length(my_crazy.list) # this reports the number of major sub-elements in the list
dim(my_crazy.list) # this won't work for complicated lists
names(my_crazy.list) <- c("one", "two", "three", "four", "five")
str(my_crazy.list) # now each part of the list has a name attribute
my_crazy.list


# 
# But this can be quite convoluted. Instead, when we create our list, we can give each element a name:
## ---- lists4, echo = TRUE, cache = TRUE----------------------------------
my_crazy.list <- list(one=vector1,two=gospels, three=my_matrix, four=TRUE, five=list.a)
str(my_crazy.list)
names(my_crazy.list)

# 
# Manipulating lists is similar to other manipulations in R, the new one is using double brackets \texttt{[[]]} to access an element of a list.
# 
## ---- lists5, echo = TRUE------------------------------------------------

# there are several ways to access/add to/subtract from a list
my_crazy.list[[1]]
my_crazy.list$one
my_crazy.list[1]
my_crazy.list["one"]
my_crazy.list$dracula <- "dracula"
my_crazy.list # now we have added another element


# We can repeat this accessing method
my_crazy.list[[3]][1,] # first row of my_matrix
my_matrix[1,]  #the same


# 
# However, you cannot do math on lists directly (note that this is set to `eval = FALSE`, since if we ran it, it throws an error and the document doesn't compile):
# 
## ---- lists6, echo = TRUE, eval = FALSE----------------------------------
## 
## my_crazy.list +2 # not so much
## my_crazy.list[[3]] + 2

# 
# \clearpage
# 
# ## Matrix operations
# Matrices are the workhorses of computational statistics. And just like there are special ways of manipulating matrices in mathematics, there are special operators in R for working with them. Unless you tell R explictly, however, it *will* operate element-wise on a matrix.
# 
## ---- matrix5, echo = TRUE, cache = TRUE---------------------------------

# A couple of matrices
H3 <- matrix(c(1, 1/2, 1/3, 1/2, 1/3, 1/4, 1/3, 1/4, 1/5), nrow=3)
H3

1/cbind(seq(1,3), seq(2, 4), seq(3,5)) # most basic function continue to be "element wise"
H3+1
H3*2
H3^2

mean(H3) # others will treat the matrix as a vector no matter what
rowSums(H3) # others work on matrices in particular ways (more on this later)
colSums(H3)
rowMeans(H3)
colMeans(H3)

# logicals too
H3==1
H3 == c(1,2,3) #what's going on here?
H3 == H3


# 
# Some functions are exact translations of math:
## ---- matrix6, echo=TRUE-------------------------------------------------

# Some work like they do in the math books
det(H3) # the determinant -- hard for you ... easy in R
diag(H3) # get the diagonal elements of amatrix
diag(1, nrow=3) # make a 3by3 identity matrix

t(H3) # matrix transpose


# 
# To access only the lower triangle of a matrix, use the `lower.tri()` function, and some indexing:
# 
## ---- matrix7, echo=TRUE-------------------------------------------------
Hnew<-H3
Hnew[lower.tri(H3, diag=TRUE)] # extract the lower triangular elements of H3

# why can we just use lower.tri()?
lower.tri(H3, diag=TRUE)

# 
# 
# To get the trace, we'll need to write a function:
## ---- matrix8------------------------------------------------------------

trace<- function(data){
  (sum(diag(data)))
}
trace(H3)

# 
# To multply matrices we use `%*%`, the matrix multiplication operator:
# 
## ---- matrix9------------------------------------------------------------

t(H3)%*%H3
c(1,2,3)%*%c(1,2,3) # dot product
matrix(c(1,2,3), ncol=1)%*%c(1,2,3) # outer product


# 
# To invert a matrix, we use the `solve()` command, which can also be used to solve a linear system:
# 
## ---- matrix10-----------------------------------------------------------
solve(H3)
invH3<-solve(H3)
H3%*%invH3 ## close enough?

# solving a linear system:
b<-c(1,2,3)

solve(H3, b)



# 
# \clearpage
# 
# # Data analysis - Roll your own linear model
# 
# You want to know: is the presidential vote share positively related to GDP growth? One way to test this is with a linear regression model:
# 
## ---- data1, cache = TRUE------------------------------------------------
library(foreign)
vote <- read.dta("votegdp.dta") # since this is in the same folder as the markdown document, we don't need a new filepath

lm(vote~q2gdp, data=vote) # sure is
coefficients <- lm(vote~q2gdp, data=vote)$coefficients


# 
# But what is this doing? First, adding a constant column to the data, for our y intercept (order matters here)! Then
# 
## ---- data2, cache = TRUE------------------------------------------------
constant <- rep(1, nrow(vote))
X <- cbind(constant, vote$q2gdp)

# 
# Then, removing the rows which have missing values:
## ---- data3, cache = TRUE------------------------------------------------


X <- X[!is.na(vote$vote),]
# na.omit(vote) # an alternative way to get rid of NA's in advance
class(X) # it's a matrix (not a data frame) so we can use our solution
is.matrix(X) # alternative approach

# 
# Creating our Y variable:
## ---- data4, cache = TRUE------------------------------------------------
Y <- cbind(vote$vote[!is.na(vote$vote)])


# 
# 
# and finally, solving $(X'X)^{-1}(X'y)$ (the OLS equation), and checking whether our results are the same as `lm()`:
## ---- data5, cache=TRUE--------------------------------------------------

B <- solve((t(X)%*%X))%*%(t(X)%*%Y)
B[1]-coefficients[1] # about zero
B[2] - coefficients[2] # about zero


# 
# \clearpage
# 
# # Flow control and functions
# 
# You will find that for many tasks your R scripts will start to get *long*.  Complex tasks will start turning into complex code.
# 
# **TIPS:**
# 
# 1. If you find yourself copying and pasting more than 2-3 times -- think about writing a loop
# or a function instead.
# 2. If you ever spend more than 20 minutes manually reshaping, editing, copying/pasting data 
# that is already encoded and on a computer --- then somewhere a fairy is killed.
# 3. Some combination of the basic skills in today's lessons can be used to solve most of these
# kinds of problems -- although it may take time work out how.
# 
# \clearpage
# 
# ## `if(){}`, `else(){}`, `ifelse(){}`
# 
# Let's start with `if()`, which works as follows:
# `if(condition){commands}`. The input in the parenthesis needs to be something that returns a logical, and tou can put anything in the braces you want:
## ---- if1----------------------------------------------------------------

if(TRUE){print("I got here")} #
if(FALSE){print("I also got here")} #

# 
# 
# You can combine `if()` with an `else()` command. Everything in the else{} braces will be executed when the condition is false
## ---- if2----------------------------------------------------------------

x <- 3
if(x>2){
  print("X is larger than 2")
} else { # notice that these are on the same line
  print("X is 2 or smaller")
}

x <- (-3)
if(x>2){
  print("X is larger than 2")
} else { # notice that these are on the same line
  print("X is 2 or smaller")
}


# 
# However, `if()` and `else()` do not play nicely with vectors, so instead we'll use `ifelse()`, a *vectorized* version of these two commands:
# 
## ---- if3, eval = FALSE--------------------------------------------------
## # This will throw an error
## if (c(1,2)>2){
##   print("This won't work")
## }

# 
#  try the `ifelse()` command instead
# 
## ---- if4----------------------------------------------------------------
x <- c(0,2)
ifelse(x > 1, "yes", "no") # but you can only put in values in here, not a bunch of instructions.

# beware though ... if your outputs are vectors it will work element-wise
yes <- c("yes1", "yes2")
no <- c("no1", "no2")
ifelse(x>1, yes, no)


# 
# 
# Note that the braces are not technically necessary if you have only a one line command, but not using them is like writing without punction. Someone can figure out what you're saying, but it makes life harder than it has to be.
# 
## ---- if5----------------------------------------------------------------

x<-3
if (x > 2) y <- 2*x else y <- 3*x
y


# 
# 
# \clearpage
# 
# ## repeat, break
# `repeat{ Execute these commands over and over again }`
# You had better include a break command in there, or you are not going to be happy. The 'break' command will stop the repeat (it will also work for the `for()` and `while()` loops below).
## ---- break1-------------------------------------------------------------

plot(NULL, xlim=c(0,100), ylim=c(0,1)) # make a blank plot with the limits set by those vectors
x <- 0
repeat {
  y <- 1/x
  x <- x+1
  points(x, y)
  if (x == 100) { break }
}

# 
# Example: make the Fibonacci series less than 300:
## ---- break2-------------------------------------------------------------
# Example: make the Fibonacci series less than 300
Fib1 <- 1
Fib2 <- 1
Fibonacci <- c(0, Fib2)
repeat{
  Fibonacci <- c(Fibonacci, Fib2)
  oldFib2 <- Fib2
  Fib2<-Fib1 + Fib2
  Fib1 <- oldFib2
  if (Fib2 > 300) {break}
}
Fibonacci


# 
# **Tip:** Save your files before running any repeat.  *Better yet...**don't** use repeat.* You should write for-loops that can stop on their own.
# 
# \clearpage
# 
# ## `while()`
#  A `while()` loop is just a repeat, where the `if(condition){break}` is specified at the top. Similar to the above:
#  
## ---- while1-------------------------------------------------------------

plot(NULL, xlim=c(0,100), ylim=c(0,1))
x <- 0
while(x < 100) {
  y <- 1/x
  x <- x+1
  points(x,y)
}

# Example 2
Fib1 <- 1
Fib2 <- 1
Fibonacci <- c(0, Fib2)
while(Fib2 <= 300){
  Fibonacci <- c(Fibonacci, Fib2)
  oldFib2 <- Fib2
  Fib2 <- Fib1 + Fib2
  Fib1 <- oldFib2
}
Fibonacci

#  
# ### Example, using the Fearon and Laitin data
# Let's say we want to calculate the average number of civil-wars onsets for each level of democratization (polity2)
# 
## ---- FL1----------------------------------------------------------------
library(foreign)
civilw <- read.dta("repdata.dta")
democracy <- min(civilw$polity2, na.rm=T)
output_vector <- NULL
index <- 1
while(democracy <= max(civilw$polity2, na.rm=T)){
  output_vector[index] <- mean(civilw$onset[civilw$polity2 == democracy], na.rm=T)
  democracy <- democracy + 1 # DON'T FORGET THIS LINE OR YOU WILL BE IN AN ENDLESS LOOP
  index <- index + 1 # this line is needed to index forward
}
output_vector*100 # civil wars start this percent of the time at each level of democracy
plot(c(min(civilw$polity2, na.rm=T):max(civilw$polity2, na.rm=T)), output_vector*100,
     xlab="Polity Scores", ylab="% Civil War Onset")

# 
# In general, this is a pretty hack-y way to calculate this, and you're better off using something like `data.table`, as discussed in lab 2.
# 
# \clearpage
# 
# ## for-loops, `next()`
# 
# The while loop still requires a lot of attention to indexing. Also ... in many instances all we want to do is increment by 1. So programmers put together a "for loop."
# 
# `for (name in vector) {execute these commands on each value of the vector}` whatever you put into the name slot will become a "local" variable
# 
## ---- for1---------------------------------------------------------------

for (monkey in c("spider", "howler", "wurst")){print(monkey)}

# or more commonly
for (i in 1:20){
  print(i)
}

# 
# ### Example of for-loops
# 
## ---- for2---------------------------------------------------------------

plot(NULL, xlim=c(0,100), ylim=c(0,1))
for (i in 0:100){points(i, 1/i)}

plot(NULL, xlim=c(0,11), ylim=c(0,11))
for(i in 1:10){
  abline(v=i)
  abline(h=i)
}

output <- NULL
for(i in 1:20){
  output[i] <- 0+i
}

# 
## ---- for3---------------------------------------------------------------
library(foreign)
civilw <- read.dta("repdata.dta")
output_vector <- NULL # Still need to instantiate the variable or you cannot index it
democracy <- sort(unique(civilw$polity2))
for (i in min(democracy):max(democracy)){
  output_vector <- c(output_vector, mean(civilw$onset[civilw$polity2 == democracy[i+11]], na.rm=T)*100)
}
plot(c(min(democracy):max(democracy)),output_vector,
     xlab="Polity Scores", ylab="% civil War Onset")


# 
# Sometimes you might not want to execute the commands for every element in the vector use the next command to skip (you can also use the break)
# 
## ---- next1--------------------------------------------------------------

some_odds <- NULL
for (i in 1:200){
  if (i%%2 != 0) {
    some_odds<-c(some_odds, i)
  } else { next }
}
some_odds


# 
# \clearpage
# 
# ## Functions
# 
# We have been using functions this whole time, but we can also make our own. This both helps you keep your code organized, and helps you better understand other people's functions. In particular, functions can be helpful if you have a repetitive serious of tasks to perform on your data.
# 
# ### Example:
# Here are three ways to write the same function:
# 
## ---- function1----------------------------------------------------------
my_function1 <- function(x){ # take in a value of x
  y<-x^2+3*x-2 # conduct some set of operations using the input values
  return(y) # return some value. In this case we are returning a simple numeric value
}

my_function2 <- function(x){
  x^2+3*x-2 # we don't have to specify a return
}

my_function3 <- function(x) x^2+3*x-2 # for simple functions, we don't even need brackets


my_function1(c(1:20))
my_function2(c(1:20))
my_function3(c(1:20))


# 
# 1 or 2 are the preferred ways to write functions, for the same reason we used braces with `if()` and `else()`: they make code more legible.
# 
# Functions are just another object.  We can define the functions using:
# 1. The word `function`
# 2. A pair of round parentheses `()` which enclose the argument list.  The list may be empty.
# 3. A single statement, or a sequence of statements enclosed in curly braces `{ }`.
# 
# 
# Sometimes a function will return a value, but other times it will just execute a command.
# 
## ---- function2----------------------------------------------------------

mad_libs <- function(noun, location, proper_noun, activity){
  print(paste("One day, I was looking all over for my", noun))
  print(paste("I decided I must have left it at the", location))
  print(paste("When I got there, I found", proper_noun, "using my", noun, "to", activity))
}
mad_libs("baseball", "lake house", "Jan Pierskalla", "eat pudding")

# 
## ---- function3----------------------------------------------------------
sqrt_plotter <- function(x){
  plot(x, sqrt(x))
}
sqrt_plotter(1:10)

# 
# 
# Other times we will want to return a value
# 
## ---- function4----------------------------------------------------------
my_abs <- function(x){
  ifelse(x<=0, -1*x, x)
}
my_abs(c(-1,2,3,-4,5))


# 
# 
# You can set default values for some of your arguments or all of them:
# 
## ---- function5----------------------------------------------------------

gaga_equation <- function(num_rah=2, num_ah=3, num_ga=2, num_la=2){
  rahs<-paste(rep("RAH", num_rah), collapse=", ")
  ahs<-paste(rep("AH", num_ah), collapse=", ")
  gas<-paste(rep("GA", num_ga), collapse=", ")
  las<-paste(rep("LA", num_la), collapse=", ")
  paste(rahs, ",", ahs,  "!  ROMA, ROMAMA!", gas,",", las)
}
gaga_equation()
gaga_equation(num_rah=5)


# Note that variables created within the function are NOT in the global environment, they don't exist "outside" of the function, and cease to exist when the function stops running. In the `gaga_equation`, you can't call `rahs` outside of the function.
# 
# 
# 
# Many of the functions you will write or work with will return lists, like the `summary` command.
# 
## ---- function6----------------------------------------------------------
library(foreign)
civilw <- read.dta("repdata.dta")


my_summary <- function(varname, data=civilw){
  attach(civilw)
  this_mean <- mean(varname, na.rm=T)
  this_var <- var(varname, na.rm=T)
  this_quant <- quantile(varname, c(.025, .25, .5, .75, .975), na.rm=T)
  detach(civilw)
  return(list(mean=this_mean, variance=this_var, quantiles=this_quant))
}
my_summary(polity2)
polity2_summary <- my_summary(polity2)
str(polity2_summary)




# 
# ## 2.6: A note on classes
# If you run `?lm`, doesn't it look like it is returning a list?
# 
## ---- class1-------------------------------------------------------------
library(foreign)
civilw <- read.dta("repdata.dta")

lm1 <- lm(polity2 ~ gdpen, data=civilw)
str(lm1) # it looks like a really complicated list
lm1[[1]] # it acts like a list
lm1$coefficients
lm1["coefficients"]
class(lm1) # But it's not a list


# 
# This is an object in the "lm" class.  In this case, it works mostly like a list. Mostly this is because some functions will work differently for objects of different classes
# 
## ---- class2-------------------------------------------------------------
print(lm1)
# print(unclass(lm1)) # that's all of the raw data



# 
# R offers the ability to modify functions based on the class of an object
## ---- class3-------------------------------------------------------------

methods(class = "lm") # these are functions that do something different for the class lm than they would for some different classes
methods(plot) # These are all the variants of the function plot.  For each of these classes of objects different sets of actual instructions are followed.


# 
# # 2.7: A note on scope
# As we noted -- local variables are not written into the global environment. Unless you are to the point where you are creating your own namespaces (in which case you can explain all of that to me), you don't need to worry much about scope except to understand that:
# 
## ---- class4-------------------------------------------------------------

# What happens in the function, stays in the function

f <-function() {
  x <- 1 #local x
  g()
  return(x)
}
g<-function(){
  x<-2 # local x
}
f()

x<-3 # global x
f()
x

# HOWEVER: you can pass values downwards in a function chain

f <-function() {
  x<- 1
  y<- g(x)
  return(c(x, y))
}
g<-function(x){
  x+2
}
f()


# 
# 
# 
# 
