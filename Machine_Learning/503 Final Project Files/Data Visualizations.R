
#################################################################
######################## Data Visualizations ####################
#################################################################

## Pairs.panels plot -- have to break it up into several sections
install.packages("psych")
require(psych)

## Pairs Panels
par(mfrow=c(1,1))
pairs.panels(Num[,c(1:10,31)], scale = TRUE)
pairs.panels(Num[,c(11:20,31)], scale = TRUE)
pairs.panels(Num[,c(21:30,31)], scale = TRUE)
## Tells us what might need a transformation

##################################
### Checking variables for logging
##################################

## Sale Price
qqnorm(Num$SalePrice)
qqnorm(log(Num$SalePrice))
## Do the transformation

## Lot Area
qqnorm(Num$LotArea)
qqnorm(Num2$LogLotArea)
## Do the transformation

###### Following log transformations make the QQ plots worse #######
## Due to the 0 values from not having a deck, porch, etc. 
### Used the log(X+0.01) trick -- to make log work

## WoodDeckSF
qqnorm(log(Num$WoodDeckSF+0.01))
qqnorm(Num$WoodDeckSF)

## OpenPorchSF
qqnorm(log(Num$OpenPorchSF+0.01))
qqnorm(Num$OpenPorchSF)

## MasVnrArea
qqnorm(Num3$MasVnrArea)
qqnorm(log(Num3$MasVnrArea+.001))

## BsmtUnfSF
qqnorm(Num3$BsmtUnfSF)
qqnorm(log(Num3$BsmtUnfSF+.001))

## Total Basement Finish
qqnorm(Num3$TotalBsmtFinsh)
qqnorm(log(Num3$TotalBsmtFinsh+.001))

#######
## Putting together the QQ plots for the final write up ##
#######

par(mar=c(3,3,4,1),mgp=c(2,0.5,0))
layout(matrix(1:6,nrow=2))
qqnorm(train$SalePrice,main="Sale Price")
qqnorm(log(train$SalePrice),main="Log Sale Price")
qqnorm(Num$LotArea, main = "Lot Area")
qqnorm(Num2$LogLotArea, main = "Log Lot Area")
qqnorm(Num$WoodDeckSF, main = "Wood Deck")
qqnorm(log(Num$WoodDeckSF+0.01), main = "Log Wood Deck SF")

par(mar=c(3,3,4,1),mgp=c(2,0.5,0))
layout(matrix(1:8,nrow=2))
qqnorm(Num$OpenPorchSF, main = "Open Porch SF")
qqnorm(log(Num$OpenPorchSF+0.01), main = "Log Open Porch")
qqnorm(Num3$MasVnrArea, main = "Masonry Veneer SF")
qqnorm(log(Num3$MasVnrArea+.001), main = "Log Masonry Veneer")
qqnorm(Num3$BsmtUnfSF, main = "SF UnFnshed Bsmt")
qqnorm(log(Num3$BsmtUnfSF+.001), main = "Log UnFnshed Bsmt")
qqnorm(Num3$TotalBsmtFinsh, main = "SF Fnshed Bsmt")
qqnorm(log(Num3$TotalBsmtFinsh+.001), main = "Log Fnshed Bsmt")


#########################################
### End of Checking variables for logging
#########################################

#########################################
### Looking at Correlation ##############
#########################################

require(corrplot)

par(mfrow=c(1,1))
correlations<- cor(Num,use="everything")
corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")


############################################################
### Redoing the visualizations with the final variables ####
############################################################
## Using Num3 instead of Num 

## Checking for shape of distributions
pairs.panels(Num3[,c(1:8,20)], scale = TRUE)
pairs.panels(Num3[,c(9:17,20)], scale = TRUE)
pairs.panels(Num3[,c(18:19,21:26,20)], scale = TRUE)

############## Correlation plot #################
## Great Graph
par(mfrow=c(1,1))
correlations2<- cor(Num3[,1:25],use="everything")
corrplot(correlations2, method="circle", type="lower",  sig.level = 0.01, insig = "blank")


##########################
## Looking at the values correlated with LogSales Price ########

par(mar=c(3,3,4,1),mgp=c(2,0.5,0))
layout(matrix(1:6,nrow=2))
plot(LogSalePrice ~ GrLivArea, data = Num3, main = "Price by General Living Area")
plot(LogSalePrice ~ Fireplaces, data = Num3, main = "Price by # of Fireplaces")
plot(LogSalePrice ~ GarageCars, data = Num3, main = "Price by # of Car Garage")
plot(LogSalePrice ~ GarageArea, data = Num3, main = "Price by Area of Garage (SF)")
plot(LogSalePrice ~ Age, data = Num3, main = "Price by Age of the House")
plot(LogSalePrice ~ SinceReMod, data = Num3, main = "Price by Years Since Remodel")
par(mfrow=c(1,1))


#################################################################
############## Boxplots of Categorical Variables ################
#################################################################

## Using train7

par(mfrow=c(2,1))
## Neighborhood
plot(train7$LogSalePrice~train7$Neighborhood, main= "Price by Neighborhood")
##zoning
plot(train7$LogSalePrice~train7$MSZoning, main = "Price by Zoning")


par(mfrow=c(2,1))
##month
plot(train7$LogSalePrice~train7$MoSold)
##overallcond 
plot(train7$LogSalePrice~train7$OverallCond)

## Checking the counts on overall condition -- strange 5 is the highest value
table(train7$OverallCond)
##OverallCond rates the OverallCond of the house

## What about overall quality?
par(mfrow=c(1,1))
plot(train7$LogSalePrice~train7$OverallQual)
## Quality is a more important measure than condition

par(mfrow=c(2,1))
##salecondition 
plot(train7$LogSalePrice~train7$SaleCondition)
##BldgType
plot(train7$LogSalePrice~train7$BldgType)


#################################################################
##################### End of Boxplots  #########################
#################################################################

