library(shiny)
library(datasets)
library(tidyverse)


library(shiny)
library(ggplot2)

ui <- fluidPage(
  headerPanel("Car Dataset"),
  
  sidebarPanel(
  selectInput("c1", "please choose column one", choices = names(mtcars)),
  selectInput("c2", "please choose column two", choices = names(mtcars)),),
  
  
  mainPanel(
  
  plotOutput("plot")),
  selectInput("c3", "choose your filter for mpg column", choices = c("max","min"),),
  tableOutput("table1"),
  verbatimTextOutput("text1")
  
)

server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(mtcars, aes(x = .data[[input$c1]], y = .data[[input$c2]])) +
      geom_point()
  })
  output$table1 <- renderTable({
    choose1= input$c3
    if(choose1=='max'){
      max(mtcars$mpg)
      
    }else{
      min(mtcars$mpg)
      
    }
  })
  output$text1 <- renderPrint(
    summary(mtcars$mpg)
  )
}

shinyApp(ui = ui, server = server)