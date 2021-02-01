#Author: Hussam Taj
#STAT 300
#Shiny App

library(shiny)
library(tidyverse)

setwd("C:/Users/Hussam Taj/Desktop/Fall 2019/STAT 300/ChangeLab Purple Posts")

master <- read.csv("PurplePostMaster.csv")
replies <- read.csv("PurplePostReplies.csv")
StudentStaticInformation <- read.csv("StudentStaticInformation.csv")
StudentTermInformation <- read.csv("StudentTermInformation.csv")

replied_participant<- unique(replies$ParticipantId)

Student_annon<- unique(master$StudentAnonId)

replied_student_annonID<-intersect(replied_participant, Student_annon)

reply_Indicator<-StudentStaticInformation%>%
  mutate(Reply=if_else(AnonStudentId %in% replied_student_annonID,1,0))


StudentTermInformation$StudentAnonId <- StudentTermInformation$StudentAnonId == unique(StudentTermInformation$StudentAnonId)

reply_Indicator1<-StudentTermInformation%>% 
  mutate(Reply=if_else(StudentAnonId %in% replied_student_annonID,1,0))


ui <- fluidPage(
  inputPanel(
    selectInput('x', 'Reply Count', choices = c("Reply"),
                selected = "Reply"),
    selectInput('y', 'Student Category', choices = c( "Reply", "GeographicRegion", "International", "RaceA", "RaceB", "RaceC", "RaceD", "RaceE", "RaceF",
                                                      "CurrentStatus", "EngagementCount", "TransferIn", "LegacyStudent", "VisitsCount"), 
                selected = "GeographicRegion")
  ),
  
  mainPanel(plotOutput("outplot"))
  
)


server <- function(input, output) {
  
  output$outplot <- renderPlot({
    ggplot(reply_Indicator, aes_string(x = input$x)) +
      geom_bar(aes_string(fill= input$y)) + 
      theme(legend.position = "top")
  })
  
}

shinyApp(ui = ui, server = server)