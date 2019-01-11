
### Loading the raw Data ####

train <- read.csv("train.csv")

#########

#### Looking at the NAs 

sapply(train, function(x) sum(is.na(x)))

# For Fence, PoolQC, MiscFeature, Fireplace, Alley, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, FireplaceQu, GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond
## NA means none of that quality -- not missing

# LotFrontage and MasVnrType
## Is just regular missing values
## Lot Frontage is the Linear Feet of Street connected to the property

train[c(8,13,15),]

## Note that 8 and 15 are corner lots -- unlikely to be a corner house and not have any frontage
## Also unlikely for a middle house

############ What to do to the variables to start out with ########

### Toss out the NAs for lotfrontage
### Modify the missing values to be new factors levels representing zero
### Still need to Transform variables.
##################

## Modifying the missing values to be new observations

train5 <- train

## Basement variables

#BsmtQual
train5$BsmtQual = factor(train5$BsmtQual, levels=c(levels(train5$BsmtQual), "NB"))
train5$BsmtQual[is.na(train5$BsmtQual)] = "NB"

#BsmtCond
train5$BsmtCond = factor(train5$BsmtCond, levels=c(levels(train5$BsmtCond), "NB"))
train5$BsmtCond[is.na(train5$BsmtCond)] = "NB"

#BsmtExposure
train5$BsmtExposure = factor(train5$BsmtExposure, levels=c(levels(train5$BsmtExposure), "NB"))
train5$BsmtExposure[is.na(train5$BsmtExposure)] = "NB"

#BsmtFinType1
train5$BsmtFinType1 = factor(train5$BsmtFinType1, levels=c(levels(train5$BsmtFinType1), "NB"))
train5$BsmtFinType1[is.na(train5$BsmtFinType1)] = "NB"

#BsmtFinType2
train5$BsmtFinType2 = factor(train5$BsmtFinType2, levels=c(levels(train5$BsmtFinType2), "NB"))
train5$BsmtFinType2[is.na(train5$BsmtFinType2)] = "NB"

## End of Basement Variables

sapply(train5, function(x) sum(is.na(x)))

## Fireplace Variable

head(train5$FireplaceQu)

#FireplaceQu
train5$FireplaceQu = factor(train5$FireplaceQu, levels=c(levels(train5$FireplaceQu), "NF"))
train5$FireplaceQu[is.na(train5$FireplaceQu)] = "NF"

## End Fireplace Variable

sapply(train5, function(x) sum(is.na(x)))

## Garage Variables

head(train5$GarageType)
##
head(train5$GarageYrBlt)
class(train5$GarageYrBlt)
##
head(train5$GarageFinish)
head(train5$GarageQual)
head(train5$GarageCond)

#GarageType
train5$GarageType = factor(train5$GarageType, levels=c(levels(train5$GarageType), "NG"))
train5$GarageType[is.na(train5$GarageType)] = "NG"

#GarageFinish
train5$GarageFinish = factor(train5$GarageFinish, levels=c(levels(train5$GarageFinish), "NG"))
train5$GarageFinish[is.na(train5$GarageFinish)] = "NG"

#GarageQual
train5$GarageQual = factor(train5$GarageQual, levels=c(levels(train5$GarageQual), "NG"))
train5$GarageQual[is.na(train5$GarageQual)] = "NG"

#GarageCond
train5$GarageCond = factor(train5$GarageCond, levels=c(levels(train5$GarageCond), "NG"))
train5$GarageCond[is.na(train5$GarageCond)] = "NG"

#GarageYrBlt
## Turning into factor levels for decades
train5$GarageYrBlt<-cut(train5$GarageYrBlt, seq(1900,2010,10), right=FALSE, labels=c("00s","10s","20s","30s","40s","50s","60s","70s","80s","90s","2ks"))
## Removing the remaining NAs -- into no garage
train5$GarageYrBlt = factor(train5$GarageYrBlt, levels=c(levels(train5$GarageYrBlt), "NG"))
train5$GarageYrBlt[is.na(train5$GarageYrBlt)] = "NG"

sapply(train5, function(x) sum(is.na(x)))

## PoolQC, Fence, MiscFeature, and Alley variables

head(train5$PoolQC)
head(train5$Fence)
head(train5$MiscFeature)
head(train5$Alley)

#PoolQC
train5$PoolQC = factor(train5$PoolQC, levels=c(levels(train5$PoolQC), "NP"))
train5$PoolQC[is.na(train5$PoolQC)] = "NP"

#Fence
train5$Fence = factor(train5$Fence, levels=c(levels(train5$Fence), "BN"))
train5$Fence[is.na(train5$Fence)] = "BN"

#MiscFeature
train5$MiscFeature = factor(train5$MiscFeature, levels=c(levels(train5$MiscFeature), "NM"))
train5$MiscFeature[is.na(train5$MiscFeature)] = "NM"

#Alley
train5$Alley = factor(train5$Alley, levels=c(levels(train5$Alley), "NA"))
train5$Alley[is.na(train5$Alley)] = "NA"

## End PoolQC, Fence, and MiscFeature variables

sapply(train5, function(x) sum(is.na(x)))

########

##############################################
######## Classes of variables ###############
###############################################

############ Looking at the classes of the dataset #############

##MSSubClass -- integer to factor
train5$MSSubClass <- as.factor(train5$MSSubClass)
levels(train5$MSSubClass)

## OverallQual-- integer to factor
class(train5$OverallQual)
train5$OverallQual <- as.factor(train5$OverallQual)
class(train5$OverallQual)
levels(train5$OverallQual)

## OverallCond  -- integer to factor
class(train5$OverallCond)
train5$OverallCond <- as.factor(train5$OverallCond)
class(train5$OverallCond)
levels(train5$OverallCond)

## MoSold  -- integer to factor
class(train5$MoSold)
train5$MoSold <- as.factor(train5$MoSold)
class(train5$MoSold)
levels(train5$MoSold)

### Name the levels ######
levels(train5$MoSold) <- c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec")

## Creating New Age Variable --- needs to come before YrSold is made into a factor
## How old the house was at sale -- skips over having to make a factor level for yrsold
## Gives more meaning as well -- hopefully improves out of sample prediction by not relying on a year but on age
train5$Age <- train5$YrSold - train5$YearBuilt
head(train5$Age)

## Creating New SinceReMod Variable -- needs to come before Yr Sold is made into a factor
## How long since the house has been remodeled
train5$SinceReMod <- train5$YrSold - train5$YearRemodAdd
head(train5$SinceReMod)

## YrSold  -- integer to factor
class(train5$YrSold)
train5$YrSold <- as.factor(train5$YrSold)
class(train5$YrSold)
levels(train5$YrSold)

### Deleting Year Built and Year remodadd to avoid linear combinations
train5$YearBuilt <- NULL
train5$YearRemodAdd <- NULL

##################################################################
##### End of Re-classifying and creating/shifting variables #############
##################################################################

##### Removing the NAs from the rest of the dataset
train6 <- train5
names(train5)
train6 <- na.omit(train5)
sum(is.na(train6))

## Adding a log transformation for price
train6$LogSalePrice <- log(train6$SalePrice)
train6$SalePrice <- NULL


###### End of removing the rows with NAs ###########

################################################################
### End of cleaning training data --- ##########################
################################################################


################################################################
################################################################
################################################################
################################################################

###############################################
####### Manipulation for Visualizations #######
###############################################


###### Extracting the numeric vectors ###########

Num<-sapply(train6,is.numeric)
Num<-train6[,Num]

###### -- log transformation of variable with lots of 0 makes QQ worse
## Ex deck sf
qqnorm(log(Num$WoodDeckSF+0.01))
qqnorm(Num$WoodDeckSF)

qqnorm(log(Num$OpenPorchSF+0.01))
qqnorm(Num$OpenPorchSF)
#######

## Creating Num2
## Removing ID and sale price from Num

Num2 <- Num

Num2$Id <- NULL
Num2$SalePrice <- NULL

## Logging lot area to improve the shape

Num2$LogLotArea <- log(Num$LotArea)
qqnorm(Num$LotArea)
qqnorm(Num2$LogLotArea)
## Log improves it


## Creating Num3
Num3 <- Num2
## Removing LotArea
Num3$LotArea <- NULL

## Creating new variables to account for the correlations and linaerity

## Bathroom variables

##Basement
Num3$BsmtBath <- Num3$BsmtFullBath + (Num3$BsmtHalfBath/2)
## Removing bsmt full and half bath
Num3$BsmtFullBath <- NULL
Num3$BsmtHalfBath <- NULL

## Total Bathrooms -- not including the basement
Num3$bath <- Num3$FullBath + (Num3$HalfBath/2)
## Deleting unecessary bathroom varaibles
Num3$FullBath <- NULL
Num3$HalfBath <- NULL


## Creating Extra Rooms variable
## TotRmsAbvGrd contains bedrooms and kitchens
## Replacing the linear combination with a new varaible and deleting the old one

Num3$ExtraRms <- Num3$TotRmsAbvGrd - (Num3$BedroomAbvGr + Num3$KitchenAbvGr)
## Deleting the tot rooms above ground
Num3$TotRmsAbvGrd <- NULL
####

## Totalbsmtfinish is a linear combo of finsf1 & finsf2
## So we will code a variable for combined finished square feet and unfinished
## Combined finished into one variable and deleting total to avoid multicollinearity
## Deleting Total basement sq, finished sf1 and finished sf2
## Creating a new variable for finished basement sf
Num3$TotalBsmtFinsh <- Num3$BsmtFinSF1 + Num3$BsmtFinSF2

## Deleting Total basement sq, finished sf1 and finished sf2

Num3$BsmtFinSF1 <- NULL
Num3$BsmtFinSF2 <- NULL
Num3$TotalBsmtSF <- NULL

## GrLivArea is a linear combination of x1stflrsf and 2ndflr sf

hh <- Num3$GrLivArea - (Num3$X1stFlrSF + Num3$X2ndFlrSF)
head(hh)
table(hh) ## Some are a little more -- none are less.

## To create the new dummy variable -- with a for loop

n <- length(Num3$LotFrontage)
Num3$X2ndFlr<-0

for (i in  1:n){
  if(Num3$X2ndFlrSF[i]>0){
    Num3$X2ndFlr[i]<-1
  }
  if(Num3$X2ndFlrSF[i]==0){
    Num3$X2ndFlr[i]<-0
  }
}


Num3$X2ndFlr <- as.factor(Num3$X2ndFlr)
levels(Num3$X2ndFlr)

head(Num3$X2ndFlr)
table(Num3$X2ndFlr)

## Deleting the 1st and 2nd flr sf variables

Num3$X1stFlrSF <- NULL
Num3$X2ndFlrSF <- NULL

names(Num3)

dim(Num3)

## Reduce the dimensions -- need to plot again

#################################################################
######## End of Manipulating the Numerical only matrix ##########
#################################################################

#################################################################
## Applying the Num transformations and manipulations to train ##
#################################################################

train7 <- train6

train7$Id <- NULL
train7$LogLotArea <- log(train7$LotArea)
## removing the lot area value
train7$LotArea <- NULL

## Creating a combined basement bathroom variable
train7$BsmtBath <- train7$BsmtFullBath + (train7$BsmtHalfBath/2)
table(train7$BsmtBath)

## Removing full and half bath
train7$BsmtFullBath <- NULL
train7$BsmtHalfBath <- NULL


## Checking linear combinations of GrLivArea and SF
hhh <- train7$GrLivArea - (train7$TotalBsmtSF + train7$X1stFlrSF + train7$X2ndFlrSF)
hh <- train7$GrLivArea - (train7$X1stFlrSF + train7$X2ndFlrSF)
head(hhh)
sum(hh)

## Use GrLivArea for 1st + 2nd SF
## Need to create a dummy variable for 2nd flr or not
### Using a for loop to create the new dummy variable

n <- length(train7$LotFrontage)
train7$X2ndFlr<-0

for (i in  1:n){
  if(train7$X2ndFlrSF[i]>0){
    train7$X2ndFlr[i]<-1
  }
  if(train7$X2ndFlrSF[i]==0){
    train7$X2ndFlr[i]<-0
  }
}
## Variable created

## Re-classifying the new variable as a factor
train7$X2ndFlr <- as.factor(train7$X2ndFlr)
levels(train7$X2ndFlr)

## Deleting the 1st and 2nd flr sf variables
train7$X1stFlrSF <- NULL
train7$X2ndFlrSF <- NULL

## Creating a new variable -- Total Bathrooms
train7$bath <- train7$FullBath + (train7$HalfBath/2)
## Deleting unecessary bathroom varaibles
train7$FullBath <- NULL
train7$HalfBath <- NULL

## Exploring the linear combination of total rooms and bedrooms and kitchens
ggg <- train7$TotRmsAbvGrd - (train7$BedroomAbvGr + train7$KitchenAbvGr)
head(ggg)
min(ggg)

## Creating a new variable -- extra rooms
## Extra rooms -- total rooms - (bedrooms + kitchens)
train7$ExtraRms <- train7$TotRmsAbvGrd - (train7$BedroomAbvGr + train7$KitchenAbvGr)
## Deleting the tot rooms above ground
train7$TotRmsAbvGrd <- NULL


## Exploring the linear combination of totalbsmtSF and individual measures of basement SF
iii <- train7$TotalBsmtSF - (train7$BsmtFinSF1 + train7$BsmtFinSF2 + train7$BsmtUnfSF)
head(iii)

## Creating a new variable for finished basement sf
train7$TotalBsmtFinsh <- train7$BsmtFinSF1 + train7$BsmtFinSF2

## Deleting Total basement sq, finished sf1 and finished sf2
train7$BsmtFinSF1 <- NULL
train7$BsmtFinSF2 <- NULL
train7$TotalBsmtSF <- NULL

names(train7)
class(train7$MoSold)
class(train7$YrSold)

#################################################################
############################### End #############################
#################################################################



#######
## Taking out Utilites
## Doesn't work with lasso -- kicks in an error about not having enough factor levels -- despite having two
train8 <- train7
train8$Utilities <- NULL

dim(train8)
#################################################################
###########Splitting the Data in Test, Train, Validate #####################
#################################################################

## Splitting train8 into train.final and validiation.final
## Already given test data as a seperate file
### Supplied test data has no sales price -- so can't use to validate

## Selecting the size of the validation set

## Using the Rule from the paper
### 11.6% should be split into validation
### The inverse proportion of free floating variables
1/sqrt(74)
1194 * 1/sqrt(74)
## 139 entries should be in the validation set 

## using Pareto's Princple
## 80 /20 split
1194 * .2
## Use 239

## Randomly seperating the training data into training and validation
all <- which(train8$LogSalePrice>0) ## Clunky Code but it works
set.seed(100)
outs <- sample(all,239) # Randomly sampling out the validation IDs
train9 <- train8[-outs,] ## Final Train
dim(train8)
validation <- train8[outs,] ## Final Validation
sum(is.na(validation))
dim(validation)


Validation2 <- na.omit(validation)

#################################################################
########################### End #################################
#################################################################

#################################################################
################ Writing the Final CSVs #########################
#################################################################

write.csv(train9, file = "Final.Train.csv",row.names=FALSE) # train9
write.csv(Validation2, file = "Final.Validation.csv",row.names=FALSE) # validation

write.csv(Num, file = "Raw.AlLNumVar.csv",row.names=FALSE) # Num
write.csv(Num3, file = "Final.AlLNumVar.csv",row.names=FALSE) # Num3

#################################################################
#################################################################
#################### End of Data Manipulation ###################
#################################################################
#################################################################