
#################################################################
#################################################################
######################## Data Analysis ##########################
#################################################################
#################################################################


#################################################################
######################## Lasso ##################################
#################################################################

install.packages("Metrics")
require(Metrics)
install.packages("pracma")
require(pracma)

library(randomForest)

library(doParallel) 
ncore = 3

install.packages("lattice")
install.packages("latticeExtra")
library(lattice)
library(latticeExtra)


## Creating the input matrix
X= sparse.model.matrix(as.formula(paste("LogSalePrice ~", paste(colnames(train9[,-68]), sep = "", collapse=" +"))), data = traintest)

## Creating the model with glmnet
## Doing a 10-fold CV
model = cv.glmnet(X[1:nrow(train9),], train9[,68], type.measure = "auc",nfolds = 10)

## Plotting the model
plot(model)
## Looks like 20 to 50 variables
plot(model$glmnet.fit,xvar = 'lambda',label = T)
## Less sure what this means 

## Looking at the best models
bestind <- which.min(model$lambda)
nextbest <- which(model$lambda==model$lambda.1se)

model$lambda[86]
model$lambda[19]

## Model with lowest MSE
rownames(model$glmnet.fit$beta)[model$glmnet.fit$beta[,bestind]!=0]

## The Variables associated with the NEXT best fit
rownames(model$glmnet.fit$beta)[model$glmnet.fit$beta[,nextbest]!=0]
## Winner
## Most parsimonious model

##########################
#### Prediction
##########################


## Using all of the lambda
pred3 = predict(model, s=model$lambda, newx=X[-(1:nrow(train9)),], type="response")

## Calculating the MSE for each lambda
msefun <- function(x,y) rmserr(x,y)$mse
mse <- apply(pred3,2,msefun,Validation2$LogSalePrice)

## Plotting the test versus training curve
plot(model)
points(log(model$lambda),mse,pch=16,col="blue",cex=.8)
legend("topleft",col=c(2,4),pch=16,legend = c("Training set","Test set"),bty="n")
## Solid performance!

## Looking at the MSE for the individual models highlighted

## Using the lambda that minimizes least squares
pred = predict(model, s='lambda.min', newx=X[-(1:nrow(train9)),], type="response")
rmserr(pred,Validation2$LogSalePrice)$mse

## Using the more parisomonious .1se
pred2 = predict(model, s='lambda.1se', newx=X[-(1:nrow(train9)),], type="response")
rmserr(pred2,Validation2$LogSalePrice)$mse


#################################################################
######################## End Lasso ##############################
#################################################################


#################################################################
######################## Random Forests #########################
#################################################################


## Creating the necessary functions to assess error

errorrate2 <- function(fit,ref) sqrt(mean((fit- ref)^2)) ## RMSE
errorrate3 <- function(fit,ref) mean(abs(fit- ref)) # MAE


#########################
#### End Tuning for ntree
#########################


## Creating the vector of ntree sizes to test
bseq=100*c(seq(2,10,2),15,20,30,50) ## B from 200 to 5000
j <- length(bseq)
## Creating a matrix to store the results
PriceBtune <- matrix(NA,nrow=dim(train9)[1],ncol=j)
### Running different B's, comparing to largest B's predictions 

## Running the tuning loop
for (a in 1:j)
{
  set.seed(321)
  PriceforB <- randomForest(LogSalePrice~.,data=train9,ntree=bseq[a])
  PriceBtune[,a] <- PriceforB$predicted
}

## Looking at how the error decreases as ntree increase
bseq
PriceBtune %>% apply(2,errorrate2,ref=PriceBtune[,j]) %>% round(3)
PriceBtune %>% apply(2,errorrate3,ref=PriceBtune[,j]) %>% round(3)
## With my sample size, looks like we need less ntree

## ntree tuning -- take two
## Using a smaller matrix of possible ntree values
## Same steps as above

bseq2=10*c(seq(2,10,2),15,20,30,50) ## B from 20 to 500 
j2 <- length(bseq)
PriceBtune2 <- matrix(NA,nrow=dim(train9)[1],ncol=j2)

for (a in 1:j)
{
  set.seed(321)
  PriceforB2 <- randomForest(LogSalePrice~.,data=train9,ntree=bseq2[a])
  PriceBtune2[,a] <- PriceforB2$predicted
}

## Looking at the errors
bseq2
PriceBtune2 %>% apply(2,errorrate2,ref=PriceBtune2[,j]) %>% round(3)
PriceBtune2 %>% apply(2,errorrate3,ref=PriceBtune2[,j]) %>% round(3)
## Looks like 100 is a good number for ntree
## My large sample size could be a factor here

#########################
#### End tuning for ntree
#########################

#########################
#### 2D tuning
#### Nodesize and Mtyr
#########################

## Starting the Parallel Processing
cl <- makeCluster(ncore, type = "SOCK")
registerDoParallel(cl)
## Creating the matrix of possible node size and # possible selected features
ftune <- expand.grid(m = c(6, 8, 10:16, 18, 20),
                     detail = c(1:4, 6))
tune1results <- foreach(a = 1:nrow(ftune),
                        .combine = 'rbind',
                        .packages = 'randomForest') %dopar% {
                          set.seed(3832)
                          tmp <- randomForest(LogSalePrice ~ ., data = train9,
                                              ntree = 100,
                                              mtry = ftune$m[a], nodesize = ftune$detail[a])
                          c(errorrate2(tmp$predicted, train9$LogSalePrice),
                            errorrate2(tmp$predicted, train9$LogSalePrice))
                        }
stopCluster(cl)
## storing the results
ftune$RMSE <- tune1results[, 1]
ftune$MAE <- tune1results[, 2]

#########################
#### End 2D tuning
#########################

########### 
##Plotting the Tuning Maps
###########

levelplot(RMSE ~ m + detail, data = ftune,
          panel = panel.levelplot.points,
          cex = 3, pch = 22,
          col.regions = rainbow(31, start = 2 / 3),
          main="Random-Forest Tuning Map: RMSE Error") +
  layer_(panel.2dsmoother(...,
                          col.regions = rainbow(31, start = 2 / 3,
                                                alpha = 0.7)))

levelplot(MAE ~ m + detail, data = ftune,
          panel = panel.levelplot.points,
          cex = 3, pch = 22,
          col.regions = rainbow(31, start = 2 / 3),
          main="Random-Forest Tuning Map: MAE Error") +
  layer_(panel.2dsmoother(...,
                          col.regions = rainbow(31, start = 2 / 3,
                                                alpha = 0.7)))

##############
## End of Plots
#############

##############
## Picking the winning tuning parameters
##############

## RMSE
ftune[which.min(ftune$RMSE), ]
##MAE
ftune[which.min(ftune$MAE), ]

## Gives the same value so 
## mtry = 20
## detail = 4

################################################
## Fitting the RF with selected tuning parameters
################################################

rf <- randomForest(LogSalePrice ~ ., data = train9,
                   ntree = 100, mtry = 20, detail = 4)
rfpred <- predict(rf, newdata = train9)
## Checking the results
plot(train9$LogSalePrice, rfpred)
##Looking at MSE
Resid <- train9$LogSalePrice - rfpred
mean(Resid^2)
## Very low MSE

## Looking at the Variable importance plot
varImpPlot(rf, main = "Relative Variable Importance")
## Overall Quality is the most important
## Neighborhood and size after that 
## Age and Exterior quality are in the next tranch
## Drops off after that

#########################
##### Prediction with the winning settings
#########################

rftestpred <- predict(rf, newdata = Validation2)

## Checking the results
plot(Validation2$LogSalePrice, rftestpred)
##Looking at MSE
Resid.2 <- Validation2$LogSalePrice - rftestpred
mean(Resid.2^2)
## Very low MSE


#################################################################
##################End of Random Forest ##########################
#################################################################



#################################################################
#################################################################
######################## End of Data Analysis ###################
#################################################################
#################################################################

