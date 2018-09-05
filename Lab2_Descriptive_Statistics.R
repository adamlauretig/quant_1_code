# POLITSC 7551, Fall 2015
# Instructor: Jan Pierskalla
# Thanks to Jacob Montgomery for sharing his original R scripts!
# Modified, Fall 2018
# TA: Adam Lauretig


#################################################
# Descriptive Statistics
#################################################

# Let's review briefly how to handle data.frames

# 1.1 Loading and accessing data.frames
library(foreign)
# Mac:
freedom <- read.dta("~/data/quant_1_code/fh.dta")
# PC:
freedom <- read.dta("F:/Quant_I/fh.dta")

freedom
head(freedom) # for a quick inspection of a data.frame
tail(freedom)
# accessing rows and columns
freedom[7,] # row 7
freedom[,2] # column 2
freedom[3,1] # row 3, column 1

# access certain variables
freedom[,2]
freedom[,"fh_pr"]
freedom$fh_pr
# access multiple variables
freedom[,c("fh_pr","fh_cl")]


# learn more about the data.frame
dim(freedom)  # dimensions of the data.frame
names(freedom) # variable names in the data.frame
colnames(freedom) # same thing
str(freedom) # variable types, names, even more details

# change variable labels in a data.frame
colnames(freedom) <- c("Country_Code","Civil_Liberties","Political_Rights")
colnames(freedom)
names(freedom) <- c("ccodewb","fh_cl","fh_pr")
colnames(freedom)[1] <- "country_code" # change the first element
colnames(freedom)[1]
rownames(freedom) # for data.frames not always interesting

# We can also 'attach' a dataset to the workspace
mean(freedom$fh_cl,na.rm=T)
attach(freedom)
search() # there it is
mean(fh_cl,na.rm=T)
detach(freedom) # we can unattach it after a while if we want

# attaching data.frames is dangerous business, don't do it!

# 1.2 Subsetting data.frames

# Sometimes you only want to work on a specific subset of the original dataset

cces <- read.dta("~/data/quant_1_code/cces.dta")
cces <- read.csv("F:/Quant_I/cces.csv") # Windows
dim(cces)
names(cces)
str(cces)
# e.g. we only want respondents born after 1970
cces_1970 <- cces[cces$birthyr>1970,]

# or 
chooser <- cces$birthyr>1970
cces_1970 <- cces[chooser, ]

# alternatively, use the subset function
cces_1970 <- subset(cces,cces$birthyr>1970)


# 1.3 Re-coding variables


freedom$fh_clpr <- (
  freedom$fh_cl+freedom$fh_pr)/2 # creates the average of both scores

# or

freedom$fh_clpr <- rowMeans(
  cbind(freedom$fh_cl, freedom$fh_pr), na.rm=TRUE) # handles missing data

# which is faster?

t1 <- Sys.time()
freedom$fh_clpr <- (
  freedom$fh_cl+freedom$fh_pr)/2 # creates the average of both scores
t2 <- Sys.time()

t3 <- Sys.time()
freedom$fh_clpr <- rowMeans(
  cbind(freedom$fh_cl, freedom$fh_pr), na.rm=TRUE) # handles missing data
t4 <- Sys.time()
t4-t3
t2-t1

# Moral: if there's a built in R command to do something, 
# that's probably faster than what you can write

# let's create a categorical variable
freedom$fh_status <- NA
freedom$fh_status <- ifelse(freedom$fh_clpr < 3,"Free",freedom$fh_status)
freedom$fh_status <- ifelse(
  freedom$fh_clpr >= 3 & freedom$fh_clpr <= 5,"Partly Free",freedom$fh_status)
freedom$fh_status <- ifelse(freedom$fh_clpr > 5,"Not Free",freedom$fh_status)

is.character(freedom$fh_status)
is.factor(freedom$fh_status) # sometimes we want to tell R that a string variable is categorical
                             # we use the factor class for that
                            

freedom$fh_status_factor <- as.factor(freedom$fh_status)


# 2.1 Numerical summaries
freedom <- read.dta("~/Dropbox/OSUClasses/Quant_I/Data/fh.dta")
freedom <- read.dta("F:/Quant_I/fh.dta")

# How could we calculate the mean of "fh_cl"?

fh_cl_mean <- sum(freedom$fh_cl)/length(freedom$fh_cl)
# ok, didn't quite work due to NAs
fh_cl_mean <- sum(freedom$fh_cl[!is.na(freedom$fh_cl)])/
  length(freedom$fh_cl[!is.na(freedom$fh_cl)])
fh_cl_mean

# let's use R's mean function
mean(freedom$fh_cl,na.rm=T)

# other summary statistics
max(freedom$fh_cl,na.rm=TRUE)
min(freedom$fh_cl,na.rm=TRUE)
median(freedom$fh_cl,na.rm=TRUE)
quantile(freedom$fh_cl,probs=c(0.25,0.5,0.75),na.rm=TRUE)

# let's have a look at the standard deviation
N <- length(freedom$fh_cl[!is.na(freedom$fh_cl)])
squared_dev <- freedom$fh_cl[!is.na(freedom$fh_cl)]-fh_cl_mean

fh_cl_sd <- sqrt((1/(N-1))*sum(squared_dev^2))
fh_cl_sd

sd(freedom$fh_cl,na.rm=TRUE)
sd(freedom$fh_pr,na.rm=TRUE)

summary(freedom)


sum_fh_cl <- c(mean(freedom$fh_cl,na.rm=TRUE),min(freedom$fh_cl,na.rm=TRUE),
  quantile(freedom$fh_cl,probs=c(0.25,0.5,0.75),na.rm=TRUE),
  max(freedom$fh_cl,na.rm=TRUE),sd(freedom$fh_cl,na.rm=TRUE))
sum_fh_cl

sum_fh_pr <- c(mean(freedom$fh_pr,na.rm=TRUE),min(freedom$fh_pr,na.rm=TRUE),
  quantile(freedom$fh_pr,probs=c(0.25,0.5,0.75),na.rm=TRUE),
  max(freedom$fh_pr,na.rm=TRUE),sd(freedom$fh_pr,na.rm=TRUE))

tab1 <- rbind(sum_fh_cl,sum_fh_pr)
colnames(tab1) <- c("Mean","Min","25th","50th","75th","Max","SD")
print(tab1)

# also
install.packages("Hmisc")
library(Hmisc)
describe(freedom)

install.packages("pastecs")
library(pastecs)

stat.desc(freedom)
stat1 <- stat.desc(freedom)[9,]
stat2 <- stat.desc(freedom)[13,]
stat3 <- stat.desc(freedom)[4,]
stat4 <- stat.desc(freedom)[5,]

tab2 <- cbind(t(stat1),t(stat2),t(stat3),t(stat4))
tab2
cbind((stat1),(stat2),(stat3),(stat4))
# Summarizing categorical variables

qog <- read.dta("~/data/quant_1_code/QoG_2010.dta")
qog <- read.dta("F:/Quant_I/QoG_2010.dta")
qog$fh_status # again a factor variable

# simple frequency table for a categorical variable
summary(qog$fh_status) # gives us the count in each category
x <- table(qog$fh_status) # same thing, but also creates a "table" object
prop.table(x) # gives us the proportions, only works on "table" objects

# you will learn how to make crosstabs further below



# 2.2 Summaries by groups

# Often you have grouped data (e.g., time-series-cross-section) and you want to 
# calculate summaries of variables over certain identifying variables

# let's load the data from Fearon and Laitin's famous 2003 article on civil wars
civilw <- read.dta("~/data/quant_1_code/repdata.dta")

install.packages("data.table")
# we're going to use data.table: it's fast, powerful, and it scales well
library(data.table)
civilw <- as.data.table(civilw)

# What if we want to know the total number of civil war onsets per country?
onsets_country <- civilw[, sum(onset, na.rm = TRUE), by = .(ccode)]
# what is this code doing?

# What about the proportion of years with a civil war onset?
prop_onsets <- civilw[, mean(onset, na.rm = TRUE), by = .(ccode)]

# We can calculate both at the same time
civilw[,. (onsets = sum(onset, na.rm = TRUE), 
  prop_onsets = mean(onset, na.rm = TRUE)), by = .(ccode)]
# what is this doing?
civilw[,. (sum(onset, na.rm = TRUE), 
 mean(onset, na.rm = TRUE)), by = .(ccode)]
# 3.1 Plots
### You will use a lot of fancy statistical techniques in in your life.  
## But conveying to readers what the data tell us about important questions is 
##not always easy to do.  One of your main goals in life will not only be to 
## collect and analyze data, but to effectively communicate your findings.  
## Graphics can help.
### There is a *lot* to learn about plotting, 
## and I am just going to give you a taste here.  
## But it will be enough for your first couple of years.
### There are two basic ways to make plots. First, there are "high-level" plots.
## Most of these are now based on the 'grid' package and many graphics I see in 
## current papers use either the 'lattice' or the 'ggplot2' packages.  
## We will go through a bit of this, although I am far from an expert.
### However, we can also make low-level plots.  
## R comes with an assortment of functions that allow us to manipulation images 
## in any way we like. As you will see, we can customize graphics to look exactly 
## how we want them. The downside is that this can take a lot of manual coding.

civilw <- read.dta("~/Dropbox/OSUClasses/Quant_I/Data/repdata.dta")
civilw <- read.dta("F:/Quant_I/repdata.dta")
civilw[1:10,]

# let's just focus on the year 1995
civilw <- subset(civilw,civilw$year==1995)
# data.table version:
civilw <- civilw[ year == 1995 ]

# 3.2 Bar plots
# Let's find out the proportion of countries that were British, French, or neither colony

colonizer <- c("Britain","France","Neither") 
colonizer_brit <- mean(civilw$colbrit)
colonizer_fra <- mean(civilw$colfra)
colonizer_prop <- c(colonizer_brit,colonizer_fra,1-colonizer_brit-colonizer_fra)

# the data.table way:
colonizer_prop <- unlist(civilw[,.(mean(colbrit), mean(colfra), 1- mean(colbrit) - mean(colfra))])
# why do we "unlist" this?

bar.colors <- c("red", "blue", "green")
barplot(colonizer_prop, names.arg=colonizer, col=bar.colors)

new.colors=c("burlywood3", "darkolivegreen", "coral3")
barplot(colonizer_prop, names.arg=colonizer, col=new.colors)
barplot(colonizer_prop, names.arg=colonizer, col=new.colors,ylim=c(0,1),ylab="Proportion of Countries",main="Bar Plot of Colonial History")

# remember, table and prop.table can calculate proportions for you if you have a categorical variable

# 3.3 The Box Plot
# We will look at the distribution of three variables using box plots
help(boxplot)
x <- civilw$pop
y <- civilw$polity2
z <- civilw$ethfrac
new.colors = c("burlywood3", "darkolivegreen", "coral3","azure3")
boxplot(x ,col = new.colors[2])
boxplot(y, col = new.colors[3])
boxplot(z, col = "orange")

###Graph Multiple Plots: Columns, Rows: ###
par(mfrow = c(1, 3))
boxplot(x ,col = new.colors[2], xlab = "Population")
boxplot(y, col = new.colors[3], xlab = "Polity 2")
boxplot(z, col = "orange", xlab = "Ethnic Fractionalization")
# population looks ugly
boxplot(log(x) ,col=new.colors[2],xlab="logged Population")
boxplot(y, col=new.colors[3],xlab="Polity 2")
boxplot(z, col="orange",xlab="Ethnic Fractionalization")


par(mfrow=c(1,1))
boxplot(civilw$ethfrac~civilw$region) # labels look funky
boxplot(civilw$ethfrac~civilw$region,names=c("West","E Europ","Asia","ME"," Africa","LA"))
boxplot(civilw$ethfrac~civilw$region,names=c("West","E Europ","Asia","ME"," Africa","LA"),horizontal=T,ylim=c(0,1))

par(mfrow=c(1,2))
boxplot(civilw$gdpenl[civilw$wars==0]~civilw$region[civilw$wars==0],names=c("West","E Europ","Asia","ME"," Africa","LA"),horizontal=T)
boxplot(civilw$gdpenl[civilw$wars==1]~civilw$region[civilw$wars==1],names=c("West","E Europ","Asia","ME"," Africa","LA"),horizontal=T)


# 3.4 The Histogram and the Density Plot

hist(civilw$polity2, main="Distribution of Polity2", xlab="Polity 2 Score", breaks=5)
abline(v=mean(civilw$polity2,na.rm=TRUE), col="red")
hist(civilw$polity2, main="Distribution of Polity2", xlab="Polity 2 Score") 
abline(v=mean(civilw$polity2,na.rm=TRUE), col="red")


par(mfrow=c(2,2))
hist(civilw$polity2, main="Distribution of Polity2", xlab="Polity 2 Score", breaks=4)
abline(v=mean(civilw$polity2,na.rm=TRUE), col="red")
rug(jitter(civilw$polity2))
hist(civilw$polity2, main="Distribution of Polity2", xlab="Polity 2 Score", breaks=8)
abline(v=mean(civilw$polity2,na.rm=TRUE), col="red")
rug(jitter(civilw$polity2))
hist(civilw$polity2, main="Distribution of Polity2", xlab="Polity 2 Score", breaks=15)
abline(v=mean(civilw$polity2,na.rm=TRUE), col="red")
rug(jitter(civilw$polity2))
hist(civilw$polity2, main="Distribution of Polity2", xlab="Polity 2 Score", breaks=50)
abline(v=mean(civilw$polity2,na.rm=TRUE), col="red")
rug(jitter(civilw$polity2))

par(mfrow=c(1,1))
plot(density(civilw$polity2,na.rm=TRUE),ylim=c(0,0.3),main="Density Plots of Polity 2 Scores",xlab="Polity2 Score")
abline(v=mean(civilw$polity2,na.rm=TRUE), col="red")
rug(jitter(civilw$polity2))
lines(density(civilw$polity2[civilw$gdpenl>6],na.rm=TRUE),col="green")
legend(-13,0.25,legend=c("All Countries","Rich Countries"),col=c("black","green"),lty=1)


# 3.5 Scatter Plots

par(mfrow=c(1,1))
plot(civilw$ethfrac,civilw$gdpenl)
plot(civilw$ethfrac,civilw$gdpenl,xlab="Ethnic Fractionalization",ylab="GDP measure")
plot(civilw$ethfrac,civilw$gdpenl,xlab="Ethnic Fractionalization",ylab="GDP measure",pch=2)
plot(civilw$ethfrac,civilw$gdpenl,xlab="Ethnic Fractionalization",ylab="GDP measure",pch=2,col="red")
plot(civilw$ethfrac,civilw$gdpenl,xlab="Ethnic Fractionalization",ylab="GDP measure",pch=2,col="red",xlim=c(0,1))
abline(lm(gdpenl~ethfrac,data=civilw))


plot(civilw$ethfrac,civilw$gdpenl,xlab="Ethnic Fractionalization",ylab="GDP measure",pch=5,col=rainbow(6)[civilw$region]
     ,xlim=c(0,1))
abline(lm(gdpenl~ethfrac,data=civilw))
#curve(-x^8+5,0,1,add=TRUE,col="red")

legend(0.6,13,legend=c("Western Democracies","Eastern Europe","Asia","ME","Africa","LA"),pch=5,col=rainbow(6))


# You can save your graphs in RStudio through the menus, or use a command

pdf(file='~/data/quant_1_code/test.pdf')
plot(civilw$ethfrac,civilw$gdpenl,xlab="Ethnic Fractionalization",ylab="GDP measure",pch=5,col=rainbow(6)[civilw$region]
     ,xlim=c(0,1))
abline(lm(gdpenl~ethfrac,data=civilw))
#curve(-x^8+5,0,1,add=TRUE,col="red")
legend(0.6,13,legend=c("Western Democracies","Eastern Europe","Asia","ME","Africa","LA"),pch=5,col=rainbow(6)
)
dev.off()


# Other packages provide variations of the basic plots

library(lattice)
bwplot(factor~numeric, ...)  #side by side boxplots
histogram( ~numeric, ...) #histograms
densityplot( ~numeric, ...) # smoothed histogram
xyplot(y~x | factor, ...) #xy plots
xyplot(civilw$gdpenl~civilw$polity2| civilw$region)
splom( ~ dataframe, ...) #scatter plot matrices



# 3.6 Handling bivariate categorical data #

smokes = c("Y", "N", "N", "Y", "N","Y","Y","Y","N", "Y") #creates vector with categorical information
amount = c(3,1,1,3,2,1,2,3,3,2) #creates vector with corresponding numerical information (1,2,3 refer to categories)
table(smokes, amount)  #tabulates both vectors

tmp <- table(smokes, amount)  #stores table
old.digits = options("digits")  #store the number of digits for safekeeping #
options(digits=3)  #only print 3 decimal places
prop.table(tmp,1)  #the rows sum to 1 now
prop.table(tmp,2)  #the columns sum to 1 now
prop.table(tmp)  # all the numbers sum to 1
options(digits=old.digits$digits)  #restores the old digits

barplot(table(smokes, amount)) #produces a barplot of smokes by amount
barplot(table(amount,smokes))  #produces a barplot of amount by smokes
barplot(table(smokes,amount),beside=T,legend.text=T) # barplot of smokes by amount, but instead of stacked besides + legend
barplot(table(amount, smokes), main="table(amount,smokes)", beside=T,legend.text=c("less than 5", "5-10", "more than 10")) #adds a legend

#Conditional Proportions 

# read the help file for table/ prop.table
prop.table(tmp) # overall proportion of each cell
prop.table(tmp,margin=1) # proportion with respect to the rows
prop.table(tmp,margin=2) # proportion with respect to the columns

# also check out ctab
install.packages("catspec")
library(catspec)
ctab(tmp)
ctab(tmp,type="row")
ctab(tmp,type="column")
ctab(tmp,type="total")

# xtab does similar stuff


########################################
### Preview of Regression
########################################
library(foreign)
library(ggplot2)

vote <- read.dta("~/data/quant_1_code/votegdp.dta")
vote <- read.dta("F:/Quant_I/votegdp.dta")

summary(vote)

## Scatterplots
plot(vote$q2gdp,vote$vote)
plot(vote$q2gdp,vote$vote,xlab="Q2 Growth",ylab="Vote Share")
plot(vote$q2gdp,vote$vote,xlab="Q2 Growth",ylab="Vote Share",pch=2)
plot(vote$q2gdp,vote$vote,xlab="Q2 Growth",ylab="Vote Share",pch=2,col="red")
plot(vote$q2gdp,vote$vote,xlab="Q2 Growth",ylab="Vote Share",pch=2,col="red",xlim=c(-10,10))


## Correlation Coefficent

# single correlation coefficient
cor(vote$q2gdp,vote$vote)
# correlation matrix
cor(vote)


### Linear Regression

# by hand
y_bar <- mean(vote$vote)
x_bar <- mean(vote$q2gdp)
s_y <- sd(vote$vote)
s_x <- sd(vote$q2gdp)
r <- cor(vote$q2gdp,vote$vote)

b <- r*(s_y/s_x)
a <- y_bar - b*x_bar
a
b
r_squared <- r^2 # R^2 is just the square of the correlation
r_squared

lm1 <- lm(vote~q2gdp,data=vote)
summary(lm1) # main results
confint(lm1) # confidence intervals for regression coefficients
confint(lm1, level=0.99) # confidence intervals for regression coefficients


## Visualizing the regression results


#plotting regression line
plot(x=vote$q2gdp,y=vote$vote,xlab="Q2 Growth",ylab="Vote Share",main="Regression Line of Q2 Growth",type="p")
abline(lm1)

#"by hand" use of abline to plot line
plot(x=vote$q2gdp,y=vote$vote,xlab="Q2 Growth",ylab="Vote Share",main="Regression Line of Q2 Growth",type="p")
summary(lm1)
#grabbing intercept and slope to put in abline statement below
lm1$coefficients
int <- lm1$coefficients[1]
int
slope <- lm1$coefficients[2]
slope
abline(a=int,b=slope,col="red")

# adding 95% confidence interval
plot(x=vote$q2gdp,y=vote$vote,xlab="Q2 Growth",ylab="Vote Share",main="Regression Line of Q2 Growth",type="p",ylim=c(35,65))
abline(lm1)
newx <- seq(min(vote$q2gdp), max(vote$q2gdp), 0.1)
a <- predict(lm1, newdata=data.frame(q2gdp=newx), interval="confidence")
lines(newx,a[,2], lty=3)
lines(newx,a[,3], lty=3) 

# or
qplot(q2gdp,vote,data=vote,geom=c("point","smooth"),method="lm")
# or
ggplot(data = vote, aes(x = q2gdp, y = vote)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# outputting regression tables to latex
mod_data <- read.dta("~/data/quant_1_code/cces.dta")

mod1 <- lm(stewart ~ pid7 + birthyr + oreilly, data=mod_data)
summary(mod1)

install.packages("xtable")
library(xtable)

tab1 <- xtable(summary(mod1))
print(tab1)

tab1 <- xtable(summary(mod1),digits=c(2,2,2,2,2))
print(tab1)

install.packages("apsrtable")
library(apsrtable)
apsrtable(mod1)
?apsrtable

mod2 <- lm(stewart ~ pid7 + birthyr, data=mod_data)
summary(mod2)

apsrtable(mod2,mod1)

# also, check out the packages stargazer and memisc


#### Bonus I: using ggplot2
install.packages("ggplot2")
library(ggplot2)
# using "fread" from data.table
osu <- fread("~/data/quant_1_code/osufb.csv") # Mac



## Base R way of managing height -----
# Ok, height is in a weird format, we need to change that
# let's convert it first to a string
# osu$Ht <- as.character(osu$Ht) 
# Now, we split it up
splits <- strsplit(osu$Ht,"-")

ft <- rep(NA,length(osu$Ht))
inch <- rep(NA,length(osu$Ht))

for (i in 1:length(osu$Ht)){
  
  ft[i] <- as.numeric(splits[[i]][1])
  inch[i] <- as.numeric(splits[[i]][2])
}

osu$height <- 30.48*ft + 2.54*inch


## more efficient way of managing height -----
osu[,.(strsplit(Ht, "-"))] # can't just do this, data.table will *store* lists

height_split <- strsplit(osu$Ht,"-")
height_matrix <- do.call(rbind, height_split)
osu[, height_ft := as.numeric(height_matrix[, 1]) * 30.48 ]
osu[, height_in := as.numeric(height_matrix[, 2]) * 2.54 ]
# or, if you want to get fancy:
osu[, `:=`(
  height_ft = as.numeric(height_matrix[, 1]) * 30.48,
  height_in = as.numeric(height_matrix[, 2]) * 2.54 
)]
osu[, height := height_ft + height_in ]


## most data.table-centric way of doing this (in 2 lines of code!)

osu[, c("height_ft", "height_in") := tstrsplit(Ht, "-")]
osu[, height := as.numeric(height_ft) * 30.48 + as.numeric(height_in) * 2.54 ]

### we can compare speeds here:
t1 <- Sys.time()
splits <- strsplit(osu$Ht,"-")

ft <- rep(NA,length(osu[,1]))
inch <- rep(NA,length(osu[,1]))

for (i in 1:length(osu[,1])){
  
  ft[i] <- as.numeric(splits[[i]][1])
  inch[i] <- as.numeric(splits[[i]][2])
}

osu$height <- 30.48*ft + 2.54*inch
t2 <- Sys.time()

t3 <- Sys.time()
osu[, c("height_ft", "height_in") := tstrsplit(Ht, "-")]
osu[, height := as.numeric(height_ft) * 30.48 + as.numeric(height_in) * 2.54 ]
t4 <- Sys.time()

(as.numeric(t2-t1))/(as.numeric(t4-t3)) # about 3.5x slower using a for-loop


# ggplot2 can make very pretty graphs. There is a lot of documentation available online


qplot(height,data=osu,geom=c("histogram"),ylab="Density",xlab="OSU Player Height",main="")
qplot(height,data=osu,geom=c("histogram"),ylab="Density",xlab="OSU Player Height",main="",binwidth=8)

qplot(Pos,Wt,data=osu,geom=c("boxplot"),ylab="Player Weight",xlab="Position",main="")+coord_flip()
qplot(Pos,Wt,data=osu,geom=c("jitter"),ylab="Player Weight",xlab="Position",main="")

qplot(height,data=osu,geom=c("density"),ylab="Density",xlab="OSU Player Height",main="")
qplot(height,data=osu,geom=c("density"),ylab="Density",xlab="OSU Player Height",main="",facets = Pos~ .)

qplot(height,Wt,data=osu,xlab="Player Height",ylab="Player Weight")
qplot(height,Wt,data=osu,xlab="Player Height",ylab="Player Weight",color=Pos)

qplot(height,Wt,data=osu,geom=c("point"),xlab="Player Height",ylab="Player Weight")
qplot(height,Wt,data=osu,geom=c("point","smooth"),xlab="Player Height",ylab="Player Weight")
qplot(height,Wt,data=osu,geom=c("point","smooth"),method="lm",xlab="Player Height",ylab="Player Weight")

osu$yr2 <- as.numeric(factor((osu$Yr)))
qplot(height,Wt,data=osu,geom=c("point","smooth"),method="lm",xlab="Player Height",ylab="Player Weight",size=yr2)+scale_size_continuous(breaks=1:4)


# Bonus: Plotting maps, see: http://journal.r-project.org/archive/2010-1/RJournal_2010-1_Weidmann+Skrede~Gleditsch.pdf 

# let's make a world map, the cshapes packages is very useful for that
library(cshapes)
library(classInt)
# load country border as of 2010
cmap.2010 <- cshp(date=as.Date("2010-1-1"))
plot(cmap.2010)

# Now, let's add Freedom House scores
# fh <- read.dta("~/Dropbox/OSUClasses/Quant_I/Data/QoG_2010.dta")


# match country identifiers from both datasets
o <- match(cmap.2010$CNTRY_NAME, qog$cname)
# order qog dataset accordingly
qog <- qog[o,]
# set row names, required for spCbind function
row.names(qog) <- cmap.2010$FEATUREID
# append using spCbind
cmap.2010.m <- spCbind(cmap.2010, qog)

# generate a color palette
pal <- grey.colors(3, 0.25, 0.95)
# find the class intervals and colors
breaks <- classIntervals(cmap.2010.m$fh_pr,n=3, style="fixed", fixedBreaks=c(1,2,5,7))
colors <- findColours(breaks, pal)
# create plot and add legend
plot(cmap.2010.m, bty="n", col=colors)
legend(x="bottomleft",legend=c("Free", "Partly Free", "Not Free"),fill = attr(colors, "palette"),
       bty = "n", title="FH Political Rights Score")

# Plotting maps with ggplot2: http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf








