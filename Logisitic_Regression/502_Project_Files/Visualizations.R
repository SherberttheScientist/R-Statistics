
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






