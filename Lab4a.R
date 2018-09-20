# An Extended example of using functions and apply()
# Taken from Jacob Montgomery

###############################################
# Better programing for a better tomorrow
###############################################
# Extended example
#### Adam Note:(We've done similar things with data table, however, this is a nice introduction to debug(), and troubleshooting)###
# Let's say that I want to write a function that runs a regression within each year.  
# All I want are the Beta estimates.

civilw <- read.dta("~/Dropbox/OSUClasses/Quant_I/Data/repdata.dta")

# First try it for one year
attach(civilw)
year.1 <- lm(polity2[year == 1983] ~ Oil[year == 1983])$coefficients[2]
detach(civilw)
year.1

# Now we might want to loop it for all years
attach(civilw)
output.vector <- NULL
for (i in unique(year)){
  output.vector[which(unique(year) == i)]<- lm(polity2 ~ Oil, data=civilw[year == i,])$coefficients[2]
}
detach(civilw)
plot(output.vector, type="l") 

# Now we might think -- there is a more general case where I want to get a bunch of 
# separate regression estimates on defined subsets of the data.

by.var.lm <- function(by.var, formula, data, coef.num){
  attach(data)
  output.vector <- NULL
  for (i in unique(by.var)){
    output.vector[which(unique(by.var) == i)]<- lm(formula, data=data[by.var == i,])$coefficients[coef.num]
  }
  detach(data)
  return(output.vector)
}
plot(by.var.lm(year, polity2~Oil, civilw, 2), type="l")


# The apply() family
# Apply functions are neat an elegant ways to get rid of loops when working with matrices.  
# This will make your code cleaner and (in some circumstances) much faster.
# In the vast majority of cases, all loops can be replaced with apply() or sappl() or mapply() or tapply() etc.

# Example
X <- matrix(rnorm(10000), ncol=100)
dim(X)
head(X)

colMax<-NULL
for (i in 1:100){
  colMax[i] <- max(X[,i])
}
colMax

# or we can just write this one line
apply(X, 2, max) # Data, margin, function
# we could do this by row by puttin '1' in the second slot
apply(X, 1, max)

# sapply() is designed to be a bit easier to use with lists
X <- (as.data.frame(X))
sapply(X, max)

# sometimes people will get clever and define their own functions right there
sapply(X, function(jj) {mean(jj)-2}) # the mean of each column - 2

# Other functions that will make your life easier (once you get a handle on them) 
# are tapply(), by(), and replicate()
# if we have time at the end of class and you want, we can go over some of these

# 4.5 browser(), debug(), traceback()
by.var.lm(polity2, onset ~ gdpenl, civilw, 2) # why won't it work?

by.var.lm <- function(by.var, formula, data, coef.num){
  attach(data)
  output.vector<-NULL
  for (i in unique(by.var)){
    output.vector[which(unique(by.var) == i)]<- lm(formula, data=data[by.var == i,])$coefficients[coef.num]
    print(i) # print so we can see where it breaking
  }
  detach(data)
  return(output.vector)
}
by.var.lm(polity2, onset ~ gdpenl, civilw, 2)

# Alternatively, pause in execution so I can mess around using browser()
by.var.lm <- function(by.var, formula, data, coef.num){
  attach(data)
  output.vector<-NULL
  for (i in unique(by.var)){
    output.vector[which(unique(by.var) == i)]<- lm(formula, data=data[by.var == i,])$coefficients[coef.num]
    browser() # pause here every time
  }
  detach(data)
  return(output.vector)
}
by.var.lm(polity2, onset ~ gdpenl, civilw, 2)
# Try looking at unique(by.var) -- that's no good

# Alternatively, pause in execution of every line using the debug() command
debug(by.var.lm)
by.var.lm(polity2, onset ~ gdpenl, civilw, 2)
undebug(by.var.lm)

## let's fix this function and move on
by.var.lm <- function(by.var, formula, data, coef.num){
  attach(data)
  output.vector<-NULL
  for (i in sort(unique(by.var))){ # sorting removes NA by default, and will put data in useful order
    output.vector[which(sort(unique(by.var)) == i)]<- lm(formula, data=data[by.var == i,])$coefficients[coef.num]
  }
  detach(data)
  return(output.vector)
}
plot(by.var.lm(polity2, onset ~ gdpenl, civilw, 2), typ="l")

# notice that we now have attached a bunch of stuff we don't want
search() # look at the data's
# usually i don't attach datasets in a function ... but you can if you want.


