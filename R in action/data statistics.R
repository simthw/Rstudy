## r in action, chapert7/8/9/10

## `chapter7` line3-191
# mtcars, 0:auto, 1:manu; 0:v-shape, 1:straight
options(digits = 2)
mydata <- mtcars[c("mpg", "hp", "wt")] # select 3 variables
head(mydata, 5) # first 5 rows
# new function for data description
mystats <- function(x, na.omit=FALSE){
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean(x) 
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n #偏度,正偏态右偏，负偏态左偏
  kurt <- sum((x-m)^4/s^4)/n-3 #峰度，大于0尖峰，小于0平峰
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}
# The distribution is skewed to the right (+0.61) 
# and is somewhat flatter than a normal distribution (–0.37)
sapply(mydata,mystats)

# descriptive methods, describe()
library(Hmisc)
describe(mydata)

# stat.desc()
library(pastecs)
stat.desc(mydata, norm=TRUE)

# describe() in package `psych`, recommend
library(psych)
describe(mydata)

# describe data by group
mystats <- function(x, na.omit=FALSE){
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean(x) 
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n #偏度,正偏态右偏，负偏态左偏
  kurt <- sum((x-m)^4/s^4)/n-3 #峰度，大于0尖峰，小于0平峰
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}
# group descriptive group using by()
dstats <- function(x)sapply(x, mystats)
by(mydata, mtcars$am, dstats) # group am: 0/1
# groups defined by multiple variables
dstats <- function(x)sapply(x, mystats)
by(mydata, 
     list(Transmission = mtcars$am,
          Engine = mtcars$vs), 
     FUN = dstats)

# using function in dplyr to describe data
library(dplyr)
library(carData)
Salaries %>%
  summarise(med = median(salary),
            min = min(salary),
            max = max(salary))
Salaries %>%
  group_by(rank, sex) %>% # grouped output by 'rank'
  summarize(n = length(salary),
            med = median(salary),
            min = min(salary),
            max = max(salary))
Salaries %>% 
  group_by(rank, sex) %>%
  select(yrs.service, yrs.since.phd) %>%
  summarise_all(mean) #nongrouping variable summary statistic

# categorical variables frequencies summarise
library(vcd)
library(openxlsx)
write.xlsx(Arthritis, "Arthritis.xlsx")
Arthritis <-read.xlsx("Arthritis.xlsx", 1) # package "openxlsx"
# tibble format
library(readxl)
Arthritis <-read_xlsx("Arthritis.xlsx", 1) # "readxl", tibble format
# table() function 
table1 <- with(Arthritis, table(Improved)) # summarize "improved" data
prop.table(table1) # proportion data
prop.table(table1) * 100 # %
# xtabs() function, 2-way table
table2 <- xtabs(~Treatment + Improved,
                 data = Arthritis) # two variables summarize
# chi-square test, null hypothesis: independence
chisq.test(table2) # "treat" vs "impro", p<0.01, reject, not independent
chisq.test(xtabs(~Sex + Improved,
                 data = Arthritis)) # "sex" vs "impro", p>0.05, independent
# fisher test, null hypothesis: independence
fisher.test(table2) # "treat" vs "impro", p<0.01
# hypothesis: Treatment and Improved are independent with each level for Sex
# p<0.01,treated improved than receiving placebos when controlling sex
mantelhaen.test(xtabs(~Treatment + Improved + Sex,
                      data = Arthritis)) 
# 2nd step measurement of relation, after reject null hypothesis
# reject independence, coefficient 0.367, larger indicate stronger relation 
assocstats(table2) 

# margin.table(), row variable
margin.table(table2, 1) # variable "treatment"
prop.table(table2, 1) # variable "treatment" ratio, sum of row=1
# column variable
margin.table(table2, 2) # variable "improved"
prop.table(table2, 2) # variable "improved" ratio, sum of column=1
# row and column
prop.table(table2) #sum of column and row=1
# sums of tables data
addmargins(table2) # add column and row 
addmargins(prop.table(table2)) # add proportion of row and column
# 1st, variable row, "treatment"; 2nd, sum row  
addmargins(prop.table(table2, 1), 2) 
# 1st, variable column, "improved"; 2nd, sum column  
addmargins(prop.table(table2, 2), 1) # 25% marked received a placebo.

# categorical variables, CrossTable() function 
library(gmodels)
CrossTable(Arthritis$Treatment,Arthritis$Improved) 

# 3-way table, ~Treatment+Sex + Improved, index1=treatment...so on
table3 <- xtabs(~ Treatment + Sex + Improved,
                data = Arthritis) # 3rd variable "improved"
ftable(table3) # recommend, compact presentation
margin.table(table3, 1) # variable "treatment"
margin.table(table3, c(1, 2)) # variable "treatment" * "sex"
# proportion with improvement for each combination
# 36% of treated males/59% of treated females, marked improvement
ftable(prop.table(table3,c(1,2))) 
ftable(addmargins(prop.table(table3,c(1,2)), 3)) # sum of index 3

# quantitative variable
write.xlsx(state.x77, "state.x77.xlsx")
# default, square matrices, 6*6 data frame
states <- state.x77[,1:6]
cov(states) # variances and covariance
cor(states) # pearson coefficient
cor(states, method = "spearman") # spearman coefficient
# nonsquare matrices coefficient
x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x, y)

# partial correlation, controling one or more variable
library(ggm)
colnames(states)
# explore 1/5 correlation, controling 2/3/6
pcor(c(1, 5, 2, 3, 6),cov(states)) #correlation: "population" vs "murder rate"

# testing correlation coefficient, null hypothesis: correlation=0
cor.test(states[,3],states[,5]) #null hypothesis: 3/5 correlation=0
# correlation matrix significance test
library(psych)
# example, "life exp" vs "illi", cor= -0.59, p=0.00, significant not 0
# as the illiteracy rate goes up, life expectancy tends to go down
corr.test(states, use = "complete")

# group comparisons
# independent(two sample) t-test, null hypothesis:two groups means are equal
library(MASS)
write.xlsx(UScrime, "UScrime.xlsx")
# variable "So": group1, south; group0, non-south
t.test(Prob~So, data=UScrime) # hypothesis:0/1, same probability, p<0.001

# Paired t-test, observations in the two groups are related
sapply(UScrime[c("U1","U2")],function(x)(c(mean=mean(x),sd=sd(x)))) 
# younger males have a higher rate
with(UScrime,t.test(U1,U2,paired=TRUE)) #same prob<0.001, reject same

## nonparametric tests
# unemployment question
sapply(UScrime[c("U1","U2")],median) 
with(UScrime,wilcox.test(U1,U2,paired=TRUE)) # alternative to paired t-test

# incarceration question
with(UScrime,by(Prob,So,median)) #group compare
wilcox.test(Prob~So,data=UScrime) #reject same hypothesis

# illiteracy question, comparision in four regions
states.x77 <- read.xlsx("state.x77.xlsx")
states2 <- data.frame(state.region, states.x77)
write.xlsx(states,"states2.xlsx")
# p<0.01, illiteracy rate isn’t same in 4 regions 
kruskal.test(Illiteracy~state.region, data=states2) 

# pairwise comparisons
source("https://rkabacoff.com/RiA/wmc.R")
wmc(Illiteracy ~ state.region, data=states, method="holm") # south 

## `chapter8` line192-383
write.xlsx(women, "women.xlsx")
# simple linear regression
fit_women <- lm(weight~height,data=women)
summary(fit_women)
women$weight #actual value
fitted(fit_women) #predicted 
residuals(fit_women) #residual error 1.525，women$weight - fitted(fit_women)
plot(women$height,women$weight,
     xlab = "height (in inches)",ylab = "weight(in pounds)")
abline(fit_women)

# regression diagnostics
par(mfrow=c(2,2)) # 2*2 plots
#independence:data collection, women weight are independent in this dataset 
#normality:QQ plot 45-degree line, residual values normally distributed, mean 0
#linearity:dependent and predictor variable, 1st plot no relation, random noise
#homoscedasticity:constant variance, random scale-location graph
plot(fit_women) 
par(mfrow=c(1,1)) #back to original


# add quadratic term, polynomial regression
fit_women2 <- lm(weight~height+I(height^2),data=women) # I()
summary(fit_women2) #residual error 0.384
plot(women$height,women$weight,
     xlab="height(in inches",ylab = "weight(in lbs)")
lines(women$height,fitted(fit_women2))

# cubic polynomial regression
fit_women3 <- lm(weight~height+I(height^2)+I(height^3),data=women)
summary(fit_women3) #residual error 0.258
plot(women$height,women$weight,xlab="height(in inches",ylab = "weight(in lbs)")
lines(women$height,fitted(fit_women3))

# multiple linear regression, > one predictor variable, 多元线性回归
df_state <- as.data.frame(state.x77[,c("Murder","Population",
                                  "Illiteracy","Income","Frost")])
#a unit change in a predictor variable, all other predictor variables constant
fit_state <- lm(Murder~Population + Illiteracy + Income + Frost,df_state)
#predictor variables account for 57% variance in murder rates across states
# model parameters and summary statistics
summary(fit_state) 
# confidence at model result
confint(fit_state)

# model comparing, p=0.99, no significant
fit_state_1 <- lm(Murder~Population + Illiteracy,df_state)
fit_state_2 <- lm(Murder~Population + Illiteracy + Income + Frost,df_state)
anova(fit_state_1, fit_state_2) # provide p value
AIC(fit_state_1, fit_state_2) #smaller AIC value is better

# stepwise regression, cant evaluate every possible model
step(fit_state, direction = "backward")

# two-predictor model Population and Illiteracy is the best
library(leaps)
leaps <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost, 
           data=states2, nbest=4)
subsTable <- function(obj, scale){ 
  x <- summary(leaps) 
  m <- cbind(round(x[[scale]],3), x$which[,-1]) 
  colnames(m)[1] <- scale 
  m[order(m[,1]), ] 
  }
subsTable(leaps, scale = "adjr2")

shrinkage <- function(fit,k=10){ #10重交叉验证
  require(bootstrap)
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  set.seed(1234)
  results <- crossval(x,y,theta.fit,theta.predict,ngroup=k)
  r2 <- cor(y,fit$fitted.values)^2
  r2cv <- cor(y,results$cv.fit)^2
  cat("Original R-square =",r2,"\n")
  cat(k,"Fold Cross-Validated R-square =",r2cv,"\n")
  cat("Change =",r2-r2cv,"\n")
} 
# using 10-fold to check 4 variables model
shrinkage(fit_state)
# check 2 variable model, better than 4 variable
shrinkage(fit_state_1)

# one-standard-deviation change cause predictor variable change
states_z <- as.data.frame(scale(df_state))
fit_states_z <- lm(Murder~Population+Illiteracy+Income+Frost,states_z) 
coef(fit_states_z) #illiteracy is the most important

# calculating relative importance of predictors
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar,2:nvar]
  rxy <- R[2:nvar,1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta^2)
  rawwgt <- lambdasq %*% beta^2
  import <- (rawwgt/rsquare)*100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import), 1, drop = FALSE]
  dotchart(import$Weights, labels = row.names(import),
           xlab = "% of R-Square", pch = 19,
           main = "Relative Importance of Predictor Variables",
           sub = paste("Total R-Square = ", round(rsquare, digits = 3)),
           ...)
  # lbls <- names(fit$model[2:nvar])
  # rownames(import) <- lbls
  # colnames(import) <- "Weights"
  # barplot(t(import),names.arg=lbls,
  #         ylab="% of R-square",
  #         xlab="Predictor Variables",
  #         main="Relative Importance of Predictor Variables",
  #         sub=paste("R-Square=",round(rsquare,digits = 3)),
  #         ...)
  return(import)
}
relweights(fit_state,col="blue") 

# qqPlot diagnostics
library(car)
qqPlot(fit_state,labels=row.names(df_state),
       id=list(method="identify"),simulate=TRUE,
       main="Q-Q plot") # "Nevada" not fitted well
states["Nevada",] #actual "nevada" murder rate,11.5%
fitted(fit_state)["Nevada"] #fitted, 3.9%
residuals(fit_state)["Nevada"] #residual, 7.6%
rstudent(fit_state)["Nevada"]
# 1st, independence of error
durbinWatsonTest(fit_state) # p,0.25,a lack of autocorrelation
# 2nd, linearity
crPlots(fit_state)
# 3rd, assessing homoscedasticity, no evidence in this example
# method1, p value 0.19,not significant, a constant variance assumption
ncvTest(fit_state) 
# a random horizontal band around a horizontal line of best fit
spreadLevelPlot(fit_state)
# 4th, evaluating multicollinearity
vif(fit_state)
vif(fit_state) > 10 # >10 indicate this problem

# multiple linear regression with interactions
write.xlsx(mtcars, "mtcars.xlsx") #mtcars dataset
fit_mtcars <- lm(mpg~hp+wt+hp:wt,data=mtcars)
summary(fit_mtcars)
# interaction plot for hp*wt, relation between mpg and hp at three values of wt
library(effects) 
plot(effect("hp:wt",fit_mtcars,
            xlevels=list(wt=c(2.2,3.2,4.2))), 
     lines = c(1, 2, 3), multiline=TRUE) 

# select poing with high leverage
hat.plot <- function(fit){
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit),main="index plot of hat values")
  abline(h=c(2,3)*p/n,col="red",lty=2)
  identify(1:n,hatvalues(fit),names(hatvalues(fit)))
}
hat.plot(fit_state)

# identify influential observations
cutoff <- 4/(nrow(states)-length(fit_state$coefficients)-2)
plot(fit_state,which=4,cook.levels=cutoff)
abline(h=cutoff,lty=2,col="red") #若设定D=1为标准，则无强影响点

# 删除某些强影响点后回归系数的改变
library(car)
avPlots(fit_state,ask=FALSE,
        d=list(method="identify"))#

# |SR|>2, outlier; x axis>0.2, high leverage; big circle, high influence
influencePlot(fit_state,id=list(method="noteworthy"),main="influence plot",
              sub="circle size is proportional to Cook's distance")

# Box–Cox transformation to normality
library(car)
summary(powerTransform(states2$Murder)) #lambda=1, p=0.14

# pop^0.8,p value 0.75, reject; illi^1.35, p value 0.54, reject
boxTidwell(Murder~Population+Illiteracy,data=states2) 

## `chapter9`
library(car)
library(rrcov)
library(multcomp)
library(effects)
library(MASS)
library(dplyr)
library(ggplot2)
# dataset cholesterol
write.xlsx(cholesterol, "cholesterol.xlsx")
# litter dataset
write.xlsx(litter, "litter.xlsx")

# one-way anova, cholesterol 
plotdata <- cholesterol %>% 
  group_by(trt) %>% 
  summarize(n = n(), 
            mean = mean(response),
            sd = sd(response), 
            ci = qt(0.975, df = n - 1) * sd / sqrt(n))
fit_chole <- aov(response ~ trt, data=cholesterol)
summary(fit_chole)
ggplot(plotdata, aes(x = trt, y = mean, group = 1)) + 
  geom_point(size = 3, color="red") + 
  geom_line(linetype="dashed", color="darkgrey") +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = .1) + theme_bw() + 
  labs(x="Treatment", y="Response", 
       title="Mean Plot with 95% Confidence Interval")
# pairwise group comparisons
pairwise <- TukeyHSD(fit_chole)
pairwise
plot_pairwise <- as.data.frame(pairwise[[1]])
plot_pairwise$conditions <- row.names(plot_pairwise)
ggplot(data=plot_pairwise, aes(x=conditions, y=diff)) +
  geom_point(size=3, color="red") + 
  geom_errorbar(aes(ymin=lwr, ymax=upr, width=.2)) + 
  geom_hline(yintercept=0, color="red", linetype="dashed") + 
  labs(y="Difference in mean levels", x="",
       title="95% family-wise confidence level") + 
  theme_bw() + coord_flip()
# multiple mean comparisons using glht()
tuk <- glht(fit_chole, linfct=mcp(trt="Tukey"))
summary(tuk)
labels1 <- cld(tuk, level=.05)$mcletters$Letters
labels2 <- paste(names(labels1), "\n", labels1)
ggplot(data=fit_chole$model, aes(x=trt, y=response)) + 
  scale_x_discrete(breaks=names(labels1), labels=labels2) +
  geom_boxplot(fill="lightgrey") +
  theme_bw() + 
  labs(x="Treatment", 
       title="Distribution of Response Scores by Treatment", 
 subtitle="Groups without overlapping letters differ significantly (p < .05)")
# test of normality assumption
qqPlot(fit_chole, simulate=TRUE, main="Q-Q Plot")
# test of homogeneity of variance
bartlett.test(response~trt,data=cholesterol)
# test of outlier
outlierTest(fit_chole) 

litter %>% 
  group_by(dose) %>% 
  summarise(n=n(), mean=mean(gesttime), sd=sd(gesttime))
fit_lit <- aov(weight~gesttime * dose,data = litter) #包含gesttime和dose交互项
summary(fit_lit) 
# group means obtained after moving effects of the covariate
effect("dose", fit_lit)

contrast <- rbind("no drug vs. drug" = c(3, -1, -1, -1))
summary(glht(fit_lit, linfct=mcp(dose=contrast)))












