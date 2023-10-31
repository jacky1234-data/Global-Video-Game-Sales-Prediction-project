library(shiny)
#install.packages('rsconnect')
#install.packages("dplyr")
#install.packages("ggplot2")
library(rsconnect)
#library(ggplot2)
#library(dplyr)
library(tidyverse)
#install.packages("showtext")
#library(showtext)
#showtext_auto()
#library(readxl)
library(data.table)
library(shinyWidgets)



genre_choices <- list("Sports","Platform","Racing","Role-Playing","Puzzle","Misc",
                   "Shooter","Simulation","Action","Fighting","Adventure","Strategy")

color <- c("Red","Yellow","Green","Blue","Pink","Orange","Purple")
color_hex <- c("red","yellow","green","blue","#FF69B4","orange","purple")
degrees <- c(1,2,3,4,5,6)

shinyUI(
  fluidPage(
    tags$head(tags$style(
      HTML('
      
        body{
          background-image:games.JPG;
        }
        
        #welcome, #end{
          color: blue; 
          font-size: 40px;
        }
        
        h2 {
          color: #28E528; 
          font-size: 30px;
        }
        
        a {
          color: #C411E1; 
          font-size: 30px;
        }
        
        #sidebar {
          background-color: #E9ED71;
          height:1200px;
        }
        
        #year, #genre{
          color:#234C99;
          font-size:20px;
        }
        
        #degs, #color{
          color:#CA16C5;
          font-size:18px;
        }
        
        #colB, #colC{
          color:#0EE121;
          font-size:16px;
        }
        
        #t1{
          color:#0EE1DA;
          font-size:20px;
        }
        
        #t2{
          color:#0EE1DA;
          font-size:20px;
        }
        
        #end{
          font-size: 35px;
        }
        
        ')
    )),
    h1(id = "welcome","Welcome to Games Sales Project"),
    h2("Here are the Summary of Game Sales (in Millions) Performance"),
    h2("Note: All Sales Includes Sales in North America, Europe, Japan, and Other Regions"),
    a(href="https://docs.google.com/spreadsheets/d/e/2PACX-1vQuGu-2VqzrTq_aQ1DTEFoiBwdbDQ9hnXXYfZOvQ22HuTHNoO0qTR5YBDkwecpJr9qqAOLUuB9-yaKw/pub?gid=1641536361&single=true&output=csv",
         "Here is the data"),
    tableOutput("table"),
    sidebarLayout(
      sidebarPanel(id="sidebar",verticalLayout(
        setSliderColor(c("DeepPink"), c(1)),
        sliderInput('year1',div(id="year","Please Select Range of Years"),min=1980,max=2020,value=c(1980,2020)),
        checkboxGroupInput("genre1",div(id="genre", "Please Select Genres"),selected=NULL, inline=TRUE,
                           choiceNames=genre_choices, choiceValues=genre_choices),
        radioButtons("color1",div(id="color","Please Select Color for the Bar"),color,"Red",TRUE),
        radioButtons("deg",div(id="degs","Please Select Degree for Model"),degrees,1,TRUE),
        radioButtons("color2",div(id = "colB","Please Select Color for the Scatter Points in the Model"),color,"Red",FALSE),
        radioButtons("color3",div(id = "colC","Please Select Color for the Fitted Line in the Model"),color,"Red",FALSE))),
        mainPanel(verticalLayout(plotOutput("p1"),plotOutput("p2"), 
                                 plotOutput("p3"),textOutput("t1"),
                                 textOutput("t2")
      )),position="left",fluid=TRUE),
    h1(id='end',"Thanks for Reading")
                
    
  )
)

