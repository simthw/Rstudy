## r in action, data treatment2/3/5/18

## `chapter2`
## 2.1 creating matrices
y <- matrix(1:20, nrow = 5, ncol = 4)
cells <- c(1, 26, 24, 68)
rnames <- c("r1", "r2")
cnames <- c("c1", "c2")
mymatriix <- matrix(cells, nrow = 2, ncol = 2, byrow = TRUE, 
                    dimnames = list(rnames, cnames)) # filled by row
mymatriix <- matrix(cells, nrow = 2, ncol = 2, byrow = FALSE, 
                    dimnames = list(rnames, cnames)) # filled by column
## 2.2 using matrix subscripts
x <- matrix(1:10, nrow = 2)
x[2,]
x[,2]

## 2.4 creating a data frame
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("type1", "type2", "type1", "type1")
status <- c("poor", "improved", "excellent", "poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata

## 2.5 specifying elements of a data frame
patientdata[1:2] #a column data
patientdata[c("diabetes", "status")]
patientdata$age #age variable
#table, cross-tabulate data, using the frame$ symbol
table(patientdata$diabetes, patientdata$status) 
# using with() instead of frame$, summary mpg, and plot mpg vs disp, wt
with(mtcars,{
  summary(mpg) 
  plot(mpg, disp) 
  plot(mpg, wt) 
})
# consider this code
with(mtcars, {
  nokeepstats <- summary(mpg) #nokeepstats is only exist within bracket
  keepstats <<- summary(mpg) #keepstats exist outside bracket
})

## 2.6 specifing factor
with(patientdata, { 
  diabetes <- factor(diabetes)
  status <- factor(status, order = TRUE)
  patientdata <- data.frame(patientID, age, diabetes, status)
  #rename 2nd column "age" to "admission age" 
  names(patientdata)[2] <- "admission age" 
  gender <- factor(patientdata$gender,
                   levels = c(1, 2), # actual variable values
                   labels = c("male", "female")) # variable value labels
  str(patientdata) #provide the object information
  summary(patientdata) #frequency counts for the categorical variables
})

## 2.7 creating a list
g <- "my first list"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow = 5)
k <- c("one", "two", "three")
mylist <- list(title = g, ages = h, j, k)
mylist[[2]] #double brackets
mylist$ages #same as double brackets
# using tibble to describe data frame
library(tibble)
mtcars <- tibble(mtcars)
mtcars[,"wt"] #returns a tibble, not a vector

## 2.3.1 import data from keyboard
#method1
mydata <- data.frame(age = numeric(0), gender = character(0),
                     weight = numeric(0) ) 
mydata <- edit(mydata)
#method2
mydatatext <- "
age gender weight
25 m 166
30 f 115
18 f 120
"
mydata <- read.table(header = TRUE, text = mydatatext) #recommend

## 2.3.2 import data from txt/csv
#comparison-1
grades1 <- read.table("studentgrade.txt", header = TRUE,
                     row.names = "StudentID", sep = ",")
str(grades1) #not conversion of character to factor
#comparison-2
grades2 <- read.table("studentgrade.txt", header = TRUE,
                     row.names = "StudentID", sep = ",",
                     colClasses = c("character", "character","character",
                                    "numeric", "numeric", "numeric"))
str(grades2) #specify class for each column, interger to numeric
#comparison-3
library(readr)
grades3 <- read_csv("studentgrade.txt") #tibble format
str(grades3)

## 2.3.3 datd from excel
library(readxl)
# forward slash "/", change windows backslash "\"
workbook <- "D:/100study/130_data/132_jianguoyun/Tools notes/R/total_10.xlsx"
xld1 <- read_xlsx(workbook, 2) #fine
str(xld1)
# comparison-2
library(readr)
# install.packages("rematch")
library(rematch)
workbook <- "D:/100study/130_data/132_jianguoyun/Tools notes/R/total_10.xlsx"
xld2 <- read_excel(workbook, 2, range = "status!A1:H27") #sheet + ! + range
str(xld2)

## `chapter3`
## listing3.1 creating leadership data frame
manager <- c(1,2,3,4,5)
date <- c("10/24/08","10/28/08","10/1/08","10/12/08","5/1/09")
country <- c("US","US","UK","UK","UK")
gender <- c("M","F","F","M","F")
age <- c(32,45,25,39,99) # wrong value
# how to score according to q1-5
q1 <- c(5,3,3,3,2)
q2 <- c(4,5,5,3,2)
q3 <- c(5,2,5,4,1)
q4 <- c(5,5,5,NA,2) # missing value
q5 <- c(5,5,2,NA,1)
leadership <- data.frame(manager,date,country,gender,age,q1,q2,q3,q4,q5,
                         row.names=manager,stringsAsFactors=FALSE)
# listing 3.2 incorporate new variables to original data frame
leadership <- transform(leadership,
                        total_score = q1 + q2 + q3 + q4 + q5,
                        mean_score = (q1 + q2 + q3 + q4 + q5)/5)
# variable[condition] <- expression, recode variable value
leadership$age[leadership$age == 99] <- NA # recoding to NA
# create age category variable
leadership$agecat[leadership$age > 75] <- "Elder"
leadership$agecat[leadership$age >= 55 &
                    leadership$age <= 75] <- "Middle aged"
leadership$agecat[leadership$age < 55] <- "Young"
# within{} function, modify data frame
leadership <- within(leadership,{ 
  agecat <- NA # create a missing column for new create agecat
  agecat[age>75] <- "Elder"
  agecat[age>=55&age<=75] <- "Middle aged"
  agecat[age<55] <- "Young"
})
# rename variable
names(leadership)[2] <- "testdate" #names(data frame)[index]

## listing3.3 applying is.na() function
is.na(leadership)

## listing3.4 using na.omit()
newdf <- na.omit(leadership) #any rows contain missing data are deleted

## listing3.7 manipulating data with dplyr
library(dplyr)
# create new variables 
leadership <- mutate(leadership,
                     total_score = q1 + q2 + q3 + q4 + q5,
                     mean_score = (q1 + q2 + q3 + q4 + q5)/5)
# recode(), modify specify variable values
leadership$gender = recode(leadership$gender, 
                           "M" = "male", "F" = "female")
# rename(), change variable name
leadership <- rename(leadership, ID = "manager", sex = "gender")
# sort using arrange(), ascending order, low to high value
leadership <- arrange(leadership, sex, total_score)
leadership_ratings <- select(leadership, ID, mean_score)
leader_men_high <- filter(leadership, 
                          sex == "male" & total_score >10)

## `chapter5`
## table5.5 normal distribution functions
library(ggplot2)
x <- seq(-3, 3, 0.1)
y <- dnorm(x) # density of x
data <- data.frame(x, y)
ggplot(data, aes(x, y)) +
  geom_line() +
  labs(x = "normal deviate", y = "density") +
  scale_x_continuous(breaks = seq(-3, 3, 1))
pnorm(1.96) # distribution area under standard curve of left z=1.96

# 90% percentile of normal distribution with mean 500 and derivation of 100
qnorm(0.9, mean = 500, sd = 100) 

# randomly create 50 numbers, mean 50, derivation 100
rnorm(50, mean = 50, sd = 10)
# pseudo-random numbers
set.seed(123) #reproduce result
runif(5)

# multivariate normal distribution
# install.packages("MultiRNG")
library(MultiRNG)
options(digits = 3) # digit numbers printed 
# 500 observations, 3 variables data
set.seed(1234)
mean <- c(230.7, 146.7, 3.6)
# variance-covariance matrix, q?
sigma <- matrix(c(15360.8, 6721.2, -47.1,
                  6721.2, 4700.9, -16.5,
                  -47.1, -16.5, 0.3), nrow = 3, ncol = 3)
muldata <- draw.d.variate.normal(500, 3, mean, sigma) # pseudo-random
muldata <- as.data.frame(muldata)
names(muldata) <- c("y", "x1", "x2")
head(muldata)

# concatenate strings, q? difference of thsee 2 expressions
cat("hello", "wzf", "\n") 
paste("hello", "wzf", sep = " ")

# function apply()
apply(muldata, 2, mean) # 2 refers to column

# listing5.6 
options(digits=2) 
student <- c("John Davis","Angela Williams","Bullwinkle Moose","David Jones",
             "Janice Markhammer","Cheryl Cushing","Reuven Ytzrhak","Greg Knox",
             "Joel England","Mary Rayburn")
math <- c(502,600,412,358,495,512,410,625,573,522)
science <- c(95,99,80,82,75,85,80,95,89,86)
english <- c(25,22,18,15,20,28,15,30,27,18)
roster <- data.frame(student,math,science,english,stringsAsFactors=FALSE)
z <- scale(roster[,c(2:4)]) # standardize variables to make them comparable
score <- apply(z, 1, mean) # row, mean standard value
roster <- cbind(roster, score) # combine mean standard 
# percentile rank of each student
y <- quantile(score, c(0.8, 0.6, 0.4, 0.2)) # range 0-1, y[1]=0.8...so on
roster$grade <- NA # creat a column "grade", values missing
roster$grade[score >= y[1]] <- "A" 
roster$grade[score < y[1]&score >= y[2]] <- "B" 
roster$grade[score < y[2]&score >= y[3]] <- "C"
roster$grade[score < y[3]&score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"
# split name, strsplit() to a vector of string returns a list,q?
name <- strsplit(roster$student, " ") 
# sapply(), "[" is a function to extract part of object, first/second name
firstname <- sapply(name, "[", 1) 
lastname <- sapply(name, "[", 2) 
# column bind, exclude "student" column
roster <- cbind(firstname, lastname, roster[,-1]) 
# sort by first and last name
roster <- roster[order(lastname, firstname),] 

# listing5.8, creat a user-written function
mystats <- function(x,parametric=TRUE,print=FALSE){
  if(parametric){ # according to parametric, use mean or use median
    center <- mean(x); spread <- sd(x)
  } else{
    center <- median(x); spread <- mad(x)
  }
  # & relation of 2 conditions
  if(print & parametric){
    cat("mean=", center, "\n", "sd=", spread, "\n")
  } else if (print & !parametric){
    cat("median=", center, "\n", "mad=", spread,"\n")
  }
  result <- list(center=center,spread=spread) # result in list
  return(result)
} 
# 500 random data
set.seed(1234)
x <- rnorm(500)
# use this function to treat data
y <- mystats(x) # y$center, or y[[1]]
y <- mystats(x, parametric = FALSE, print = TRUE)

# example of switch() function, using parameter "type" for indicating date format
mydate <- function(type = "long") { 
  switch(type,
         long = format(Sys.Date(), "%A %B %d %Y"),
         short = format(Sys.Date(), "%m-%d-%Y"),
         cat(type, "is not a recognized type\n")
  )
}
mydate("long") # type=long, date format "%A %B %d %Y"
mydate("short") # type = short, data format "%m-%d-%Y"
mydate()
mydate("medium") # not match long or short

# listing5.10, converting wide format to a long format
library(tidyr)
data_wide <- data.frame(ID = c("AU", "CN", "PRK"), 
                        Country = c("Australia", "China", "North Korea"),
                        LExp1990 = c(76.9, 69.3, 69.9), 
                        LExp2000 = c(79.6, 72.0, 65.3), 
                        LExp2010 = c(82.0, 75.2, 69.6))
data_long <- gather(data_wide, 
                    key="Variable", 
                    value="Life_Exp", 
                    c(LExp1990, LExp2000, LExp2010))

# listing5.11, converting long format to wide format
data_wide <- spread(data_long, key=Variable, value=Life_Exp)

# listing5.13, aggregating data
aggdata <-aggregate(mtcars[-c(2, 10)], 
                    # 1st column = mtcars$cyl, and 2nd
                    by=list(Cylinders=mtcars$cyl, Gears=mtcars$gear), 
                    FUN=mean, na.rm=TRUE)

## `chapter18`
## advanced missing data treatment
# install.packages("missForest")
library("VIM")

# load data in package
data(sleep, package = "VIM")
options(digits = 2)
# complete data in sleep dataset, 42*10 
com_sleep <- sleep[complete.cases(sleep),] 
# one or more missing in a row, 20*10
incom_sleep <- sleep[!complete.cases(sleep),] 
# evaluate information about missing data
sum(is.na(sleep$Dream))
mean(is.na(sleep$Dream)) # ratio of incomplete in column "dream"
mean(!complete.cases(sleep)) # ration of imcomplete in "sleep"

# present missing value in table and graph
library("mice")
md.pattern(sleep, rotate.names = T)

# aggr() to show missing values in dataset
library("VIM")
aggr(sleep, prop = F, numbers = T) # prop = proportions

# matrixplot() presnet missing value
matrixplot(sleep, sort="BodyWgt") # [0,1], not proper

# 
x <- as.data.frame(abs(is.na(sleep)))
# extract missing value
y <- x[which(apply(x,2,sum)>0)]
# missing variables correlations (column) between observed variables (row)
cor(sleep,y,use="pairwise.complete.obs")

# exclude missing values, relation of dream to span and gest
fit <- lm(Dream ~ Span + Gest, data = na.omit(sleep))
# dream~span(control gest);dream~gest(control span)
summary(fit)

# pairwise deletion, not proper method
cor(sleep, use = "pairwise.complete.obs") # use all available observations

# impute missing data, kNN()function
library(VIM)
sleep_imp <- kNN(sleep, imp_var = F) # F suppress additional added columns

# impute missing data, missForest()
library("missForest")
set.seed(1234)
sleep_imp2 <- missForest(sleep)$ximp # returns a list, ximp refer dataset

# multiple imputation
library("mice")
data(sleep, package = "VIM")
imp <- mice(sleep, seed = 1234) # list dataset
# regression coefficient, p for span, 0.07, not significant; <0.01 for gest
fit <- with(imp, lm(Dream ~ Span + Gest)) # list dataset
pooled <- pool(fit)
summary(pooled)
complete(imp, action = 3) # 3rd complete dataset by multiple imputation 