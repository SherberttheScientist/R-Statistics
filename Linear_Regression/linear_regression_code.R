# STAT R 501 Final Project 
## Edited Code

###Installing necessary packages

##testing for hetreskedascity
install.packages("lmtest",repos = 'http://cran.us.r-project.org')
library(lmtest)
install.packages("car",repos = 'http://cran.us.r-project.org')
library(car)

setwd("~/Desktop/R Workspace/501")

## Loading the data
Project <- read.csv("ProjectData2.csv")
names(Project)

#######################
### Brief Cleaning
#######################

head(Project$Rainfall) ## in mm
head(Project$Rainfall.2) ## in inches
## want to use Inches becuase mm is too granular of a measurement
Project$Rainfall <- Project$Rainfall.2 # Changing rainfall to inches
Project$Rainfall.2 <- Project$Rainfall^2 ## adding the squared term

Project$Population <- NULL # Not of interest -- more people equal more drinking
## This study also doesn't take into acccount traveling, etc.
## So population may not reflect true numbers of drinkers

Project$GDP <- as.numeric(Project$GDP)

## Creating a matrix for automated regression
Project2 <- Project

Project2$country <- NULL
Project2$beer_servings <- NULL
Project2$spirit_servings <- NULL
Project2$wine_servings <- NULL

## Log Transformation for unemployment

min(Project2$unemployment.rate)

Project2$unemployment.rate2 <- log(Project2$unemployment.rate) ## log transformation
Project2$unemployment.rate <- NULL

#######################
### End Cleaning
#######################




########################################################################
########################################################################
########################## Data visualization ########################
########################################################################
########################################################################

## Visualizing the relationship between variables of 
## inferenical interest

plot(Project$Total.Alcohol~Project$Rainfall, main = "Does Rainfall lead to Drinking?", xlab="Rainfall (inches)",ylab = "Total Alcohol Consumed (Liters)")
## Need titles and commentary

## This study is a very focused inferential study.
##Visualizing the variables of main inferenical interest

par(mfrow=c(1,1))

hist(Project$Total.Alcohol, freq= FALSE, main = "Histogram of Total Alcohol Consumed (liters)", xlab="Alcohol",col="grey")
curve(dnorm(x,mean=mean(Project$Total.Alcohol),sd=sd(Project$Total.Alcohol)),col="red",add=TRUE)

hist(Project$Rainfall, freq = FALSE, main = "Histogram of Rainfall (inches)", xlab="Rainfall",col="grey")
curve(dnorm(x,mean=mean(Project$Rainfall),sd=sd(Project$Rainfall)),col="red",add=TRUE)

###################
## Put in Appendix
###################

## Looking at total.alcohol
plot(Project$Total.Alcohol~Project$unemployment.rate)
plot(Project$Total.Alcohol~log(Project$unemployment.rate))

par(mfrow=c(1,2))
qqnorm(Project$Total.Alcohol, main = "Total.Alcohol")
qqline(Project$Total.Alcohol)
qqnorm(log(Project$Total.Alcohol), main = "Log Transformation")
qqline(log(Project$Total.Alcohol))


## Looking at unemployment.rate
plot(Project$Total.Alcohol~Project$unemployment.rate, main = "Unemployment")
plot(Project$Total.Alcohol~log(Project$unemployment.rate), main="Log Unemployment")

qqnorm(Project$unemployment.rate, main = "Unemployment")
qqline(Project$unemployment.rate)
qqnorm(log(Project$unemployment.rate), main= "Log Transformation")
qqline(log(Project$unemployment.rate))
### Log unemployment rate

## Looking at GDP

par(mfrow=c(1,1))
plot(Project$Total.Alcohol~Project$GDP, main = "Alcohol Consumption by Income", xlab="GDP",ylab = "Alcohol Consumed (Liters)")
## No need for any transformations or quad


## Looking at Literacy
par(mfrow=c(1,2))
plot(Project$Total.Alcohol~Project$Literacy, main = "Literacy Rate", xlab="Literacy Rate",ylab = "Alcohol Consumed (Liters)")
plot(Project$Total.Alcohol~log(Project$Literacy), main = "Log Transformation", xlab="Log Literacy Rate",ylab = "Alcohol Consumed (Liters)")

qqnorm(Project$Literacy, main = "Literacy Rate")
qqline(Project$Literacy)
qqnorm(log(Project$Literacy), main = "Log Transformation")
qqline(log(Project$Literacy))
## Leave it alone
## No need for a quadratic terms


## Looking at temp

par(mfrow=c(1,1))
plot(Project$Total.Alcohol~Project$Temp, main = "Alcohol Consumption by Temp", xlab="Temperature (C)",ylab = "Alcohol Consumed (Liters)")
## No transformation or quadratic

#########################
### End of Appendix plots
#########################


############
### Boxplots
############

## Looking at Island

boxplot(Project$Total.Alcohol~Project$Island, ylab = "Liters of Alcohol", xlab = "1 = Island",main = "Do Islands drink more?")

## Looking by Region

boxplot(Project$Total.Alcohol~Project$Region, main = "Regional Drinking",ylab = "Liters of Alcohol")

###################
### End of Boxplots
###################



########################################################################
########################################################################
########################## Data Analysis ########################
########################################################################
########################################################################

#####################################
##Fitting the linear regression model
#####################################

Simple <- lm(Project$Total.Alcohol~Project$Rainfall)

#################################
## Fitting of the quadratic model
#################################

quadratic.model <-lm(Project$Total.Alcohol ~ Project$Rainfall + Project$Rainfall.2)

##################################
##Visualizing the regression lines
##################################

par(mfrow=c(1,1))
plot(Project$Total.Alcohol~Project$Rainfall, main = "Does Rainfall lead to Drinking?", xlab="Rainfall (inches)",ylab = "Total Alcohol Consumed (Liters)")
abline(Simple, col="blue")
cq <- coef(quadratic.model)
rainvalues <- seq(0, 174, 1)
sp.quad = cq[1] + cq[2]*rainvalues +cq[3]*rainvalues^2 
lines(rainvalues,sp.quad, col='red') 

## Checking for normality
par(mfrow=c(1,2))
qqnorm(resid(Simple), main= "Simple Model")
qqline(resid(Simple))
qqnorm(resid(quadratic.model), main = "Quadratic Model")
qqline(resid(quadratic.model))


###############################
##Summary of regression results
###############################

### Note: a simple linear regression is equal to 
### a one sample t test

summary(Simple)
summary(quadratic.model)

################################
## End of the regression results
################################

#################################################################################
#################################################################################
#################################################################################
#################################################################################



##Testing for hetroskedascity visually

plot(fitted(Simple),residuals(Simple), main="Simple Model Residual Plot") ## Hints that we should be using a quasi-poisson model
plot(fitted(quadratic.model),residuals(quadratic.model), main="Quadratic Model Residual Plot") ## Use a quasi-poisson model

## Testing for Hetrosked
###Breusch Pagan test

bptest(Simple)
bptest(quadratic.model)
#### Looks like including the quadratic term helps the heteroskedascity.
#### Still looks off visually though

###########################
## Correcting for Hetrosked
###########################

## White's corrected errors
coeftest(Simple,vcov=hccm(Simple))

## Using glms

## quasi-Poisson

Simple.P <- glm(Project$Total.Alcohol~Project$Rainfall,family = poisson(link = log))
summary(Simple.P)

##Comparing models

## Squared term

Null <- glm(Project$Total.Alcohol~1,family=gaussian)
Quad.P <- glm(Project$Total.Alcohol~Project$Rainfall+Project$Rainfall.2,family = gaussian)

summary(Null)
summary(Quad.P)

###########################################################
##Secondary Analysis: Confounding and Interacting Variables
###########################################################

##Temperature
p1 <- glm(Total.Alcohol~Rainfall*Temp*Rainfall.2,data=Project)
summary(p1)

p2 <- glm(Total.Alcohol~Rainfall+Rainfall.2+Temp,data=Project)
summary(p2)


#######################
#######################
#######################
#######################
#######################


Test <- glm(Total.Alcohol~Rainfall*Temp * Rainfall.2, data = Project)
summary(Test)
bptest(Test)
plot(Test$fitted.values,Test$residuals)

Test.P <- glm(Total.Alcohol~Rainfall*Temp * Rainfall.2, data = Project,family = poisson(link = "log"))
summary(Test.P)
plot(Test.P$fitted.values,Test.P$residuals)

##########
###########


##############
## Full Model
##############


Project2$GDP <- as.numeric(Project2$GDP)
Project2$Region <- NULL
Full.Int <- glm(Total.Alcohol~(.)^2,data=Project2)

summary(Full.Int)


plot(Full.Int$fitted.values,Full.Int$residuals)

bptest(Full.Int)
##

##########################
#End of secondary analysis
##########################

###############################
##Secondary Analysis: Linearity
###############################

