
## Loading the data
data <- read.csv("mpg_dataset.csv")

###########
## Step one
###########

str(data) # Looking at the variables and their classes

## Creating an ordered table of median hwy mpg
step_one <- aggregate(data$hwy ~ data$manufacturer, FUN = median)
colnames(step_one) <- c("Manufacturer", "Highway_MPG")
ordered_step_one <- step_one[order(-step_one$Highway_MPG), ]

print(ordered_step_one, row.names = FALSE) 

##Creating a "significant range"
median_highway <- median(data$hwy)
sd_highway <- sd(data$hwy)
range_highway <- median_highway + c(-1,1) * sd_highway
range_highway
## It looks like honda and volkswagon are leading the way with jeep - land rover being below average

## Graphing the relationship

library(ggplot2)

## A boxplot will allow us to view the breadth of the hwy_mpg per manufacturer
p <- ggplot(data, aes(manufacturer, hwy))
p + geom_boxplot() + ggtitle("Highway MPG by Manufacturer")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs( x = "Manufacturers", y = "Highway MPG")

#### Stat test

## simple regression -- against the H0 of average hwy mpg

hwy_reg <- glm(hwy ~ manufacturer, data = data)
summary(hwy_reg)

###############
### Step Two
###############

str(data$model) # going to be hard to graph

newdata <- data[ , c(2,8,9)] ## Temporary data frame for ease of visualization

pairs(newdata) # Looking at all scatterplots


#### Correlation between highway and city

p_2 <- ggplot(newdata, aes(hwy,cty))
p_2 + geom_point() + ggtitle("City MPG vs. Highway MPG") + labs(x = "Highway MPG", y = "City MPG")

cor(newdata$cty, newdata$hwy) ## Very highly correlated

##########
## Next two boxplots are not used in the write up
#########

## Boxplot of city mpg by model

p_3 <- ggplot(newdata, aes(model, cty))
p_3 + geom_boxplot() + ggtitle("City MPG by Model")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs( x = "Model", y = "City MPG")

## Boxplot of highway mpg by model 

p_4 <- ggplot(newdata, aes(model, hwy))
p_4 + geom_boxplot() + ggtitle("Highway") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "Model", y = "Highway MPG")

#### Side by Side boxplot

## Melting the data to make a side-by-side boxplot
require(reshape2)
data.melt <- melt(newdata, id.var = "model")
head(data.melt)
tail(data.melt)

## A side by side boxplot
ggplot(data = data.melt, aes(x=model, y=value)) + geom_boxplot(aes(fill=variable)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "Model", y = "MPG")

####################

## Looking at the best and worst performing models

## Creating an aggregated table of median values by model
step_two_table <- aggregate(data[, 8:9], list(data$model), median)

## Adding in a column for the difference in performance
step_two_table_full <- cbind(step_two_table, round(step_two_table[, 3] - step_two_table[, 2], 2))
## Adding col names
colnames(step_two_table_full) <- c("Model", "City", "Highway", "Hwy - Cty")
## Viewing the table
step_two_table_full

## Figuring out which are the best and which are the worst

##Creating a "significant range" for cty
median_city <- median(data$cty)
sd_city <- sd(data$cty)
range_city <- median_city + c(-1,1) * sd_city
range_city

## The highway was calculated above
range_highway

### Break these into good and bad city and good/bad highway

best <- step_two_table_full[(step_two_table_full$City > 21 & step_two_table_full$Highway > 29.5), ]
best

worst <- step_two_table_full[(step_two_table_full$City < 12.5 & step_two_table_full$Highway < 18.0), ]
worst

#####

## The best and worst models should be the same for each individual category due to the high correlation
best_city <- step_two_table_full[(step_two_table_full$City > 21), ]
best_city

best_highway <- step_two_table_full[(step_two_table_full$Highway > 29.5), ]
best_highway

worst_city <- step_two_table_full[(step_two_table_full$City < 12.5), ]
worst_city

worst_highway <- step_two_table_full[(step_two_table_full$Highway < 18.0), ]
worst_highway
## There are more cars of the worst_highway than the worst_total

######## Plotting just the worst and best

best_worst <- rbind(best, worst)
best_worst <- best_worst[,1:3]
melt_best_worst <- melt(best_worst, id.var = "Model")

## Plotting the median hwy and city mpg values per best and worst models
p_5 <- ggplot(data = melt_test, aes(x=Model, y=value)) 
p_5 + geom_bar(stat = "identity", aes(fill=variable), position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "Model", y = "MPG") + ggtitle("MPG by Model")
######

######## Not included in the write up -- too crowded
## Looking at all of the MPG for each model
all_three <- data[, c(2,8,9)]
all.melt <- melt(all_three, id.var = "model")
ggplot(data = all.melt, aes(x=model, y=value)) + geom_bar(stat = "identity", aes(fill=variable), position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#### Regressions
model_city <- lm(cty ~ model, data = data)
summary(model_city)

model_hwy <- lm(hwy ~ model, data = data)
summary(model_hwy)
########

########
## End
#########