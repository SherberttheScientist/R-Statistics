install.packages("GGally")
install.packages("wesanderson")
library(ggplot2)
library(GGally)
library(dplyr)
library(broom)
library(wesanderson)

Project.final <- read.csv("FinalProjectData502.csv")

##Sorry for the data cleaning, my structure won't carry over after I write a csv file.

Project.final$X <- NULL
Project.final$Survived %<>% as.factor
Project.final$Pclass %<>% as.factor
Project.final$spouse %<>% as.factor
Project.final$children %<>% as.factor
Project.final$parents %<>% as.factor
Project.final$siblings %<>% as.factor
levels(Project.final$Age2) <- c("30-60","6-12","60-80","19-30","13-18","0-5")
Project.final$Age2 <- relevel(Project.final$Age2, ref = "19-30")
Project.final$Age2 <- relevel(Project.final$Age2, ref = "13-18")
Project.final$Age2 <- relevel(Project.final$Age2, ref = "6-12")
Project.final$Age2 <- relevel(Project.final$Age2, ref = "0-5")
levels(Project.final$Age2)
print(str(Project.final))


ui12 <- fluidPage(
  radioButtons("xInput", label = h4("X axis"),
               choices = list("Survived", "Sex", "Pclass", "Age2"),  
               selected = "Survived"),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  plotOutput("bar")
)

server12 <- function(input,output){
  
  
  output$bar<- renderPlot({
    ggplot(Project.final,aes(x=get(input$xInput),fill=factor(Survived)))+theme_economist_white()+
      geom_bar(stat='count',position='dodge')+
      ggtitle("Fate of the Passengers on the Titanic",subtitle = paste(input$xInput,"First?"))+
      labs(x=input$xInput,y="# of People")+ 
      scale_fill_manual(values=c("dodgerblue", "darkorange2"))
  })
}

shinyApp(ui = ui12, server = server12)
