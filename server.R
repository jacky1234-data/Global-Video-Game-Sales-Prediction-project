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
library(kableExtra)


data <- read.csv("videogamesale.csv")

x_power <- c("","x","x^2","x^3","x^4","x^5","x^6")


genre_choices <- c("Sports","Platform","Racing","Role-Playing","Puzzle","Misc",
                      "Shooter","Simulation","Action","Fighting","Adventure","Strategy")

fill = c("Europe"="blue","North America"="red","Japan"="green","Others"="yellow")

color <- c("Red","Yellow","Green","Blue","Pink","Orange","Purple")
color_hex <- c("red","yellow","green","blue","#FF69B4","orange","purple")

summerize_data <- function(columns,data){
  summerized_data <- data %>%
    group_by_at(columns) %>%
    summarize(NA_Total = sum(NA_Sales),EU_Total = sum(EU_Sales),
              JP_Total = sum(JP_Sales),Other_Total = sum(Other_Sales),
              NA_Portion = sum(NA_Sales)/(sum(NA_Sales)+sum(EU_Sales)+sum(JP_Sales)+sum(Other_Sales)),
              EU_Portion = sum(EU_Sales)/(sum(NA_Sales)+sum(EU_Sales)+sum(JP_Sales)+sum(Other_Sales)),
              JP_Portion = sum(JP_Sales)/(sum(NA_Sales)+sum(EU_Sales)+sum(JP_Sales)+sum(Other_Sales)),
              Other_Portion = sum(Other_Sales)/(sum(NA_Sales)+sum(EU_Sales)+sum(JP_Sales)+sum(Other_Sales)),
              All_Sales = sum(NA_Sales)+sum(EU_Sales)+sum(JP_Sales)+sum(Other_Sales))
  return(summerized_data)
}

selection <- function(selected,vector){
  if (length(selected)==0){
    return(vector)
  } else{
    return(selected)
  }
}



shinyServer(
  function(input, output, session) {
    
    selected_genre <- reactive({selection(input$genre1, genre_choices)})
    filtered_data <- reactive({filter(data, (Year>=input$year1[1] & Year<=input$year1[2] 
                                             & Genre %in% selected_genre()))})
    summary_filtered_data_header <- reactive({summerize_data(c(),filtered_data())})
    overall_data <- reactive({data.frame("North America" = c(summary_filtered_data_header()$NA_Total[1],
                                                   round(summary_filtered_data_header()$NA_Portion[1]*100,2)),
                               "Europe" = c(summary_filtered_data_header()$EU_Total[1],
                                                   round(summary_filtered_data_header()$EU_Portion[1]*100,2)),
                               "Japan" = c(summary_filtered_data_header()$JP_Total[1],
                                                   round(summary_filtered_data_header()$JP_Portion[1]*100,2)),
                               "Others" = c(summary_filtered_data_header()$Other_Total[1],
                                                   round(summary_filtered_data_header()$Other_Portion[1]*100,2)),
                               row.names = c("Sales","Percentage of All Sales"))})
    
    # overall_data2 <- reactive({overall_data() %>%
    #     kbl() %>%
    #     kable_paper("hover", full_width = F)})
        
    
    output$table <- function(){
      overall_data() %>%
        kbl() %>%
        kable_styling() %>%
#        kable_material(c("striped", "hover")) %>%
        row_spec(0, color = "black",bold = T, background = "#A4EF54", font_size = 20) %>%
        row_spec(1, color = "white",background = "#14dbbd", font_size = 25) %>%
        row_spec(2, color = "white",background = "#10c8e8", font_size = 25) %>%
        column_spec(1:5, border_left = TRUE)
    }

    # output$table <- renderTable({overall_data()},rownames=TRUE)
    
    summary_filtered_data_p1 <- reactive({summerize_data(c("Year"),filtered_data())})
    
    output$p1 <- renderPlot({ggplot(data=summary_filtered_data_p1(),aes(x=Year))+
        geom_line(aes(y=EU_Total,color="Europe"))+
        geom_line(aes(y=NA_Total,color="North America"))+
        geom_line(aes(y=JP_Total,color="Japan"))+
        geom_line(aes(y=Other_Total,color="Others"))+
        labs(x='Years',y="Total Sales",title = "Total Sales verus Time by Regions",color = "Regions")+
        theme_linedraw()+
        theme(
          plot.title = element_text(color="#760AF0", size=20, face="bold"),
          axis.title.x = element_text(color="#EA1445", size=18, face="bold"),
          axis.title.y = element_text(color="#110EE1", size=18, face="bold"),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          legend.title = element_text(color="black", size=14),
          legend.text = element_text(color="black", size=14)
        ) +
        scale_color_manual(values = fill)})
    
    summary_filtered_data_p2 <- reactive({summerize_data(c("Genre"),filtered_data())})
    color_pos1 <- reactive({which(input$color1 == color)[[1]]})
    real_color1 <- reactive({color_hex[color_pos1()]})
    
    output$p2 <- renderPlot({ggplot(data=summary_filtered_data_p2(),aes(x=Genre,y=All_Sales))+
        geom_col(fill=real_color1())+
        theme_linedraw()+
        theme(
          plot.title = element_text(color="#760AF0", size=20, face="bold"),
          axis.title.x = element_text(color="#EA1445", size=18, face="bold"),
          axis.title.y = element_text(color="#110EE1", size=18, face="bold"),
          axis.text.x = element_text(angle=60,size=16),
          axis.text.y = element_text(size=16)
        ) +
        labs(x='Genres',y="Total Sales", title = "Total Sales versus Genre")})
    
    summary_filtered_data_p3 <- reactive({summerize_data(c("Year"),filtered_data())})
    summary_filtered_data_p3_edited <- reactive({summary_filtered_data_p3() %>%
                            mutate(Year = Year-1980)})
    color_pos2 <- reactive({which(input$color2 == color)[[1]]})
    real_color2 <- reactive({color_hex[color_pos2()]})
    color_pos3 <- reactive({which(input$color3 == color)[[1]]})
    real_color3 <- reactive({color_hex[color_pos3()]})
    degree <- reactive({as.integer(input$deg)})
    output$p3 <- renderPlot({ggplot(data=summary_filtered_data_p3_edited(),aes(x=Year,y=All_Sales))+
        geom_point(color=real_color2())+
        theme_linedraw()+
        theme(
          plot.title = element_text(color="#760AF0", size=20, face="bold"),
          axis.title.x = element_text(color="#EA1445", size=18, face="bold"),
          axis.title.y = element_text(color="#110EE1", size=18, face="bold"),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16)
        ) +
        geom_smooth(method = 'lm', formula = y ~ poly(x,degree()), color=real_color3())+
        labs(x='Years Since 1980',y="Total Sales",title="Total Sales versus Time with a Polynomial Model")})
    
    poly_model <- reactive({lm(summary_filtered_data_p3_edited()$All_Sales 
                               ~ poly(summary_filtered_data_p3_edited()$Year,degree(),raw = TRUE))})
    r_square <- reactive({as.numeric(summary(poly_model())$r.squared)})
    coef_data <- reactive({as.data.frame(summary(poly_model())$coefficients)$Estimate})
    coef_length <- reactive({length(coef_data())})
    eqn <- "y ="
    output$t1 <- renderText({
      for (i in 1:coef_length()){
        if (coef_data()[i]>0){
          if (i==1){
            eqn <- paste(eqn, round(coef_data()[i], 3), x_power[i], collapse = "")
          } else{
            eqn <- paste(eqn, "+", round(coef_data()[i], 3), x_power[i], collapse = "")
          }
        } else{
          eqn <- paste(eqn, "- ", round(coef_data()[i] * (-1), 3), x_power[i], collapse = "")
        }
      }
      paste("The Fitted Equation is", eqn, collapse=" ")
      })
    output$t2 <- renderText({paste("The R Square is", round(r_square(),3), collapse=" ")})
    
  }
  
)