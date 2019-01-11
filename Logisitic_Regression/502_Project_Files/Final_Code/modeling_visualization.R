
#############################################################################################################################################################################################################
####################VISUALIZATIONS##################################################################################################################

Project <- read.csv("Project.502")

View(Projectraw)

###Women First?###

ggplot(Project,aes(x=Sex,fill=factor(Survived)))+theme_economist_white()+
  geom_bar(stat='count',position='dodge')+
  ggtitle("Fate of the Passengers on the Titanic",subtitle = "Women First?")+
  labs(x="Sex",y="# of People")+ 
  scale_fill_manual(values=c("dodgerblue", "darkorange2"))

####Women First by percentage?##

ggplot(Project,aes(x=Sex,fill=factor(Survived)))+theme_economist_white()+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ggtitle("Fate of the Passengers on the Titanic",subtitle = "Women First?")+
  labs(x="Gender",y="% of People")+ 
  scale_fill_manual(values=c("dodgerblue", "darkorange2"))

####

###Child or not



Project$child %<>% as.factor

ggplot(Project,aes(x=child,fill=factor(Survived)))+theme_economist_white()+
  geom_bar(stat='count',position='dodge')+
  ggtitle("Fate of the Passengers on the Titanic",subtitle = "Children First?")+
  labs(x="Child (=1)",y="# of People")+ 
  scale_fill_manual(values=c("dodgerblue", "darkorange2"))

####Child First by percentage?##
##Not very helpful
ggplot(Project,aes(x=child,fill=factor(Survived)))+theme_economist_white()+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  ggtitle("Fate of the Passengers on the Titanic",subtitle = "Children First?")+
  labs(x="Child (=1)",y="% of People")+ 
  scale_fill_manual(values=c("dodgerblue", "darkorange2"))


###Children First? A granular approach###

levels(Project$Age2) <- c("(0,5]","(5,12]","(12,18]","(18,30]","(30,60]","(60,80]")
ggplot(Project,aes(x=Age2,fill=factor(Survived)))+theme_economist()+
  geom_bar(stat='count',position='dodge')+
  ggtitle("Fate of the Passengers on the Titanic",subtitle = "Children First?")+
  labs(x="Age Group",y="# of People")+ 
  scale_fill_manual(values=c("dodgerblue", "darkorange2"))

##had (5,12] in the wrong spot
# moved this to the data manipulation area 



####

###Women and children?###

ggplot(Project,aes(x=Sex,fill=factor(Survived)))+theme_economist()+
  geom_bar(stat='count',position='dodge')+facet_wrap(~Age2)+
  ggtitle("Fate of the Passengers on the Titanic",subtitle = "Female Children First?")+
  labs(x="Sex",y="# of People")+ 
  scale_fill_manual(values=c("dodgerblue", "darkorange2"))

###

###By Class

ggplot(Project,aes(x=Pclass,fill=factor(Survived)))+theme_fivethirtyeight()+
  geom_bar(stat='count',position='dodge')+
  ggtitle("Fate of the Passengers on the Titanic")+
  labs(x="Socio-economic Class",y="# of People")

###


###By Class and Sex



ggplot(Project,aes(x=Sex,fill=factor(Survived)))+theme_fivethirtyeight()+
  geom_bar(stat='count',position='dodge')+facet_wrap(~Pclass)+
  ggtitle("Fate of the Passengers on the Titanic")+
  labs(x="Gender",y="# of People")

###Use p2, it is more illuminating
##Muliplot function##

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##
multiplot(p1, p2, cols=2)



ggplot(Project3, aes(x=Fare,y=Survived))+theme_tufte()
  geom_point()

ggplot(Project3, aes(x=Fare,y=Survived))+theme_tufte() +
  geom_line()+
  ggtitle("Pay More and Live?")

install.packages("ggthemes")
library(ggthemes)
###



?table

table(Project$Pclass,Project$Sex)
table(Project$Pclass,Project$Age2)


####################################################################################################################################################################################################################################MODELING##############################################################################################################################################################################################

str(Project)
str(Project$Survived)

Project$Survived %<>% as.factor
Project$spouse %<>% as.factor

Project2 <- Project

Project2$PassengerId <- NULL
Project2$Name <- NULL
Project2$Age <- NULL
Project2$SibSp <- NULL
Project2$Parch <- NULL
Project2$Ticket <- NULL
Project2$Cabin <- NULL

Project3 <- Project2

Project2$Age2 <- NULL
Project3$child <- NULL


Project$Pclass %<>% as.factor
Project2$Pclass %<>% as.factor
Project3$Pclass %<>% as.factor

Project$children %<>% as.factor
Project2$children %<>% as.factor
Project3$children %<>% as.factor

Project$siblings %<>% as.factor
Project2$siblings %<>% as.factor
Project3$siblings %<>% as.factor

Project$parents %<>% as.factor
Project2$parents %<>% as.factor
Project3$parents %<>% as.factor

Project2$Survived %<>% as.factor
Project3$Survived %<>% as.factor

Project2$spouse %<>% as.factor
Project3$spouse %<>% as.factor

Project2$child %<>% as.factor

##Releveling factors

Project2$Sex <- relevel(Project2$Sex, ref = "male")
levels(Project3$Sex)
Project3$Sex <- relevel(Project3$Sex, ref = "male")
levels(Project3$Sex)


Project2$Pclass <- relevel(Project2$Pclass, ref = "2")
Project3$Pclass <- relevel(Project3$Pclass, ref = "2")
levels(Project3$Pclass)

str(Project)
dim(Projectraw)
##no interactions

mod.NoInt <- glm(Survived ~ ., family = binomial (link='logit'),data=Project2)
summary(mod.NoInt)

inv.logit(8.640e-01)
inv.logit(8.655e-01)
inv.logit(2.705)
inv.logit(1.253)



#Fitting the Null model - using project2 dataset - it has a binary for child or not but doesn't have an predictor for age.

mod.null <-glm(Survived ~ 1, family = binomial (link='logit'),data=Project2)

#Full Model

mod.full <- glm(Survived ~ .*., family = binomial (link='logit'),data=Project2)

##Step AIC
#From the Null

mod.for = stepAIC(object = mod.null, scope = ~ (Pclass + Sex + Fare + Embarked + spouse + children+siblings + parents + child)^2,
                  direction = "forward")
summary(mod.for)

#From the Full

mod.back = stepAIC(object = mod.full, direction = "backward")
summary(mod.back)

mod.back.modified <- glm(formula = Survived ~ Pclass + Sex + spouse + 
                           siblings + parents + child + Pclass:Sex + Pclass:spouse + 
                           Pclass:child + Sex:siblings + Sex:child, 
                         family = binomial(link = "logit"), data = Project2)

AIC(mod.back,mod.back.modified)
#the Step AIC model is better

#Going both ways

mod.both.full = stepAIC(object = mod.full, scope = ~ (Pclass + Sex + Fare + Embarked + spouse + children+siblings + parents + child)^2,
                        direction = "both")
summary(mod.both.full)

mod.both.null = stepAIC(object = mod.null, scope = ~ (Pclass + Sex + Fare + Embarked + spouse + children+siblings + parents + child)^2,
                        direction = "both")
summary(mod.both.null)


#my best guess at a model
mod.t <- glm(formula = Survived ~ Pclass + Sex + spouse + 
               siblings + children + child + Pclass:Sex + 
               Pclass:child + Sex:child + parents:child, 
             family = binomial(link = "logit"), data = Project2)
summary(mod.t)


##Comparing models

AIC(mod.t,mod.for,mod.back,mod.both.full,mod.both.null)

summary(mod.back)
coef(mod.back)[c(1:2,4,20:21,33)]

?coef
###Mixed effects modeling

lmm <- lmer(formula = Survived ~ 1+Pclass + Sex + Fare + Embarked + spouse + 
              (1|siblings) + parents + child + Pclass:Sex + Pclass:spouse + 
              Pclass:child +  Sex:parents + Sex:child + Fare:spouse + Fare:parents + Fare:child + parents:child, 
            family = binomial(link = "logit"), data = Project2,
            REML = FALSE)
summary(lmm)


AIC(mod.back,lmm)

?lmer



#####

##Fixing the factor levels of Age2
str(Project3$Age2)

Project3$Age2 <- as.factor(as.numeric(Project3$Age2))
levels(Project3$Age2) <- c("Young Child","Child","Teenager","Fit Adult","Adult","Elderly")
Project3$Age2 <- relevel(Project3$Age2, ref = "Fit Adult")
##no interactions
str(Project3)
mod.NoInt2 <- glm(Survived ~ ., family = binomial (link='logit'),data=Project3)
summary(mod.NoInt2)


#Fitting the Null model - using project3 dataset - it has a categorical age variable

mod.null2 <-glm(Survived ~ 1, family = binomial (link='logit'),data=Project3)

#Full Model

mod.full2 <- glm(Survived ~ .*., family = binomial (link='logit'),data=Project3)

##Step AIC
#From the Null

mod.for2 = stepAIC(object = mod.null2, scope = ~ (Pclass + Sex + Fare + Embarked + spouse + children+siblings + parents + Age2)^2,
                  direction = "forward")

#From the Full

mod.back2 = stepAIC(object = mod.full2, direction = "backward")

##Both ways

mod.both.full2 = stepAIC(object = mod.full2, scope = ~ (Pclass + Sex + + Fare + Embarked + spouse + children+siblings + parents + Age2)^2,
                        direction = "both")
summary(mod.both.full2)

mod.both.null2 = stepAIC(object = mod.null2, scope = ~ (Pclass + Sex + Fare + Embarked + spouse + children+siblings + parents + Age2)^2,
                        direction = "both")

#my best guess

mod.t2 <- glm(formula = Survived ~ Pclass + Sex + spouse + 
               siblings + children + Age2 + Pclass:Sex + 
               Pclass:Age2 + Sex:Age2,
             family = binomial(link = "logit"), data = Project3)
summary(mod.t2)

AIC(mod.t2,mod.for2,mod.back2,mod.both.full2,mod.both.null2,lmm4)

summary(mod.for2)

###Mixed effects modeling


lmm4 <- lmer(Survived ~ Sex + Pclass + Age2 + Sex:Pclass + 
               Sex:Age2 + Pclass:Age2+ (1|siblings), family = binomial(link = "logit"), 
             data = Project3,
             REML = FALSE)

summary(lmm4)
Anova(lmm4)

AIC(lmm4,lmm5)


lmm5 <- lmer(Survived ~ Sex + Pclass + Age2 + Sex:Pclass + 
                (1|siblings), family = binomial(link = "logit"), 
             data = Project3,
             REML = FALSE)

Anova(lmm5)

AIC(lmm4,lmm5)

#mixed effects dont add anything

mod.final <- mod.for2

summary(mod.final)

summary(mod.final)

Project3$siblings3


##a quick binomial fit
##Doesn't improve the model
tt <- glm(formula = Survived ~ Sex + Pclass + Age2 + siblings3 + Sex:Pclass + 
      Sex:Age2 + Sex:siblings3 + Pclass:Age2, family = binomial(link = "logit"), 
    data = Project3)

summary(tt)

AIC(mod.final,tt)


########################


table(Project3$Age2)
table(Project3$siblings)
table(Project3$Pclass)

Project3$siblings %<>% as.numeric
Project3$siblings <- Project3$siblings - 1

Project3$siblings2 <- rep(0,length(Project3$siblings))

for (i in 1: length(Project3$siblings2)){
  if (Project3$siblings[i]>2){
    Project3$siblings2[i] <- 3}
  else {
    Project3$siblings2[i] <- Project3$siblings[i]
  }
  }

Project3$siblings3 <- rep(0,length(Project3$siblings))

for (i in 1: length(Project3$siblings3)){
  if (Project3$siblings[i]>0){
    Project3$siblings3[i] <- 1}
  else {
    Project3$siblings3[i] <- 0
  }
}


Project3$siblings <- Project3$siblings + 1
Project3$siblings %<>% as.factor
Project3$siblings2 %<>% as.factor
Project3$siblings3 %<>% as.factor

str(Project3)

lmm6 <- glmer(Survived ~ Sex + Pclass + Age2 + Sex:Pclass + 
               Sex:Age2 + Pclass:Age2+ (1|siblings2), family = binomial(link = "logit"), 
             data = Project3)


lmm7 <- glmer(Survived ~ Sex + Pclass + Age2 + Sex:Pclass + 
                Sex:Age2 + Pclass:Age2+ (1|siblings), family = binomial(link = "logit"), 
              data = Project3)


AIC(mod.for2,lmm6,lmm7,GHQ)



summary(lmm6)

?glmer



summary(mod.back)
Estimate <- coef(mod.back)[c(1:2,4,20:21,33)]
d <- confint(mod.back, c(1:2,4,20:21,33), level = 0.95)
e <- cbind(Estimate,d)
e
g2 <- e[c(4,6),]*-1
h2 <- e[c(1:3,5),]

positive2 <- inv.logit(h2)
f <- inv.logit(g2)
negative2 <- f * -1



inv.logit(-3.134929)



summary(mod.t4)
Estimate2 <- coef(mod.t4)[c(1:3,10:11,13,25)]
d2 <- confint(mod.t4, c(c(1:3,10:11,13,25)), level = 0.95)
e2 <- cbind(Estimate2,d2)
g <- e2[c(1,4,5,7),]*-1
h<- e2[c(2,3,6),]

positive <- inv.logit(h)

f2 <- inv.logit(g)
negative <- f2 * -1

inv.logit(4.8075)



Project3
mod.t4 <- glm(formula = Survived ~ Sex + Pclass + Age2 + Sex:Pclass + 
                Sex:Age2 + Pclass:Age2, family = binomial(link = "logit"), 
              data = Project3)
summary(mod.t4)

inv.logit(3.6)

table(Project3$Age2,Project3$Sex,Project3$Survived)



AIC(mod.t2,mod.for2,mod.back2,mod.both.full2,mod.both.null2,lmm4)
