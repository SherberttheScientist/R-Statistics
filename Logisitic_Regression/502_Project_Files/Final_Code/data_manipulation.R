#Stat R 502 Final Project
# Data manipulation and cleaning
Projectraw <- read.csv("train.csv")
Titanic.data <- read.csv("train.csv")

View(Projectraw)
dim(Projectraw)
Titanic.data %<>% na.omit

str(Titanic.data)

##making the response variable a categorical response.

Titanic.data$Survived %<>% as.factor

##making class a factor

Titanic.data$Pclass %<>% as.factor

View(Titanic.data)



###########################Scratch code doesn't work####################################
# Trying to recode sibsp and parch
if age>18 then spouse = sibsp
else spouse = 0
if age <18 then siblings = sibsp
else siblings = 0

if age >18 then children = parch
else children = 0
if age <18 then parents = parch
else parents = 0


###########################End of Scratch code########################################################################

names(Titanic.data)


##Spouse

Titanic.data$spouse <- rep(0,length(Titanic.data$Sex))


for (i in 1:length(Titanic.data$spouse)){
  if (Titanic.data$Age[i]>20){
    Titanic.data$spouse[i] <- Titanic.data$SibSp[i]
  }
  else {
    Titanic.data$spouse[i] <- 0
  }
}

Titanic.data$spouse %<>% as.factor

#getting rid of the multiple spouses - they represent people at the cutoff age that have siblings on board.

for (i in 1:length(Titanic.data$spouse)){
  if (Titanic.data$spouse[i]>1){
    Titanic.data$spouse[i] <- 0
  }
}

max(Titanic.data$spouse)

#Spouse finished


#Start of coding the siblings variable.

Titanic.data$siblings <- rep(0,length(Titanic.data$Sex))

for (i in 1:length(Titanic.data$siblings)){
  if (Titanic.data$Age[i]<20){
    Titanic.data$siblings[i] <- Titanic.data$SibSp[i]
  }
  else {
    Titanic.data$siblings[i] <- 0
  }
}

for (i in 1:length(Titanic.data$siblings)){
  if (Titanic.data$SibSp[i]>1){
    Titanic.data$siblings[i] <- Titanic.data$SibSp[i]
  }
}

##Finished coding siblings

##coding children
if age >18 then children = parch
else children = 0

Titanic.data$children <- rep(0,length(Titanic.data$Sex))

for (i in 1:length(Titanic.data$children)){
  if (Titanic.data$Age[i]>20){
    Titanic.data$children[i] <- Titanic.data$Parch[i]
  }
  else {
    Titanic.data$children[i] <- 0
  }
}

for (i in 1:length(Titanic.data$children)){
  if (Titanic.data$Parch[i]>2){
    Titanic.data$children[i] <- Titanic.data$Parch[i]
  }
}

#finished coding children

#coding parents
if age <18 then parents = parch
else parents = 0

Titanic.data$parents <- rep(0,length(Titanic.data$Sex))

for (i in 1:length(Titanic.data$parents)){
  if (Titanic.data$Age[i]<20){
    Titanic.data$parents[i] <- Titanic.data$Parch[i]
  }
  else {
    Titanic.data$parents[i] <- 0
  }
}

for (i in 1:length(Titanic.data$parents)){
  if ((Titanic.data$Age[i]<20 & Titanic.data$SibSp[i]>1)){
    Titanic.data$parents[i] <- Titanic.data$Parch[i]
  }
}

#Finished coding parents

View(Titanic.data)

###On to the age variable

##Age groups
Titanic.data$Age2 <- cut(Titanic.data$Age,c(0,5,12,18,30,60,80))

str(Titanic.data$Age2)

View(Titanic.data)

levels(Titanic.data$Age2) <- c("(0,5]","(5,12]","(12,18]","(18,30]","(30,60]","(60,80]")

##Binary

Titanic.data$child <- rep(0,length(Titanic.data$Sex))

for (i in 1:length(Titanic.data$child)){
  if (Titanic.data$Age[i]<15){
    Titanic.data$child[i] <- 1
  }
  else {
    Titanic.data$child[i] <- 0
  }
}

str(Titanic.data$child)

Titanic.data$child %<>% as.factor

##End of Age

str(Titanic.data)

View(Titanic.data)

#creating the output file

?write.csv

write.csv(Titanic.data,file = "Project.502")


###########Final Data set

Project.final <- Project3

str(Project.final)
Project.final$siblings2 <- NULL
Project.final$siblings3 <- NULL
str(Project.final)

write.csv(Project.final,file="FinalProjectData502.csv")
