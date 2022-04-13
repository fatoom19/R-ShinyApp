library(tidyverse)
library(markdown)
library(DT)
library(shiny)
#Read
df<-read.csv("Life_Expectancy.csv")
#View(df)
library(dplyr)
#Find null value
which(is.na(df))
df1<-df %>% na_if("")
#Eliminate all null values

df<- drop_na(df1)

#View(df)
# checking data
summary(df)
# After checking data, it shows The minimum BMI equal to 1. Which doesn't make sense
# The lowest BMI that has ever recorded equals 7.5.(source:https://edinstitute.org/blog/2011/12/8/part-v-fat-no-more-fear-no-more-contempt)
DS<-df %>% filter(BMI >= 7.5)
df<- drop_na(DS)

keys <- colnames(df)[-c(1,2,3)]
values <- c('Life Expectancy (age in years)','Adult Mortality Rates (Deaths per 1000 population)','Number of Infant (Deaths per 1000 population)',
            'Alcohol consumption in litres','Expenditure on health as a percentage of GDP(%)','(HepB) immunization coverage among 1-year-olds (%)','reported cases per 1000 population','Average Body Mass Index of entire population',
            'Number of under-five deaths per 1000 population','(Pol3) immunization coverage among 1-year-olds (%)','government expenditure on health as a percentage of total government expenditure (%)','(DTP3) immunization coverage among 1-year-olds (%)',
            'Deaths per 1000 live births HIV/AIDS (0-4 years)','Gross Domestic Product per capita (in USD)','Population of the country','Prevalence of thinness among children and adolescents for Age 10 to 19 (%)',
            'Prevalence of thinness among children for Age 5 to 9(%)','Human Development Index','Number of years of Schooling(years)')

description <- list()                  # Create empty list
for(i in 1:length(keys)) {              # Add key/value pairs in for-loop
  description[keys[i]] <- values[i]}


library(shinythemes)


ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  "Life Expactancy Dataset from World Health Organisation",
                  tabPanel("Visualization",
                           mainPanel(
                             headerPanel("Analyzing Life Expectancy Dataset"),
                             
                             sidebarPanel(
                               selectInput("c1", "please choose column one", choices = names(df)[-c(1,2,3)]),
                               selectInput("c2", "please choose column two", choices = names(df)[-c(1,2,3)]),),
                             
                             mainPanel(
                               radioButtons("PlotColor", "Please Select Plot Color:",
                                            c("blue", "black", "red"), selected ='blue'),
                               textInput("title1",("Enter plot title"), value = ""),
                               
                               sliderInput("Plotsize", "Please Select size of plots",
                                           min = 1, max = 5, value = 2),
                               plotOutput("plot")),
                           ) # mainPanel
                           
                  ),
                  #############################
                  
                  #############
                  tabPanel("Functions",                              
                           selectInput("c3", "choose a column to apply a function", choices =names(df)[-c(1,2,3)],),
                           radioButtons("fun", label = h3("Select Function"),
                                        choices = list("Max" = 1, "Avaerage" = 2, "Min" = 3), 
                                        selected = 2),
                           verbatimTextOutput("text1"))
                  , # Navbar 1, tabPanel
                  ##########################
                  tabPanel("The Data",
                           
                           h2("The Life Expactincy Data"),
                           DT::dataTableOutput('table'),
                  ),
                  ####################
                  tabPanel("About", includeMarkdown("readme.Rmd")
                  )
                  
                  
                  
                )# navbarPage
)# fluidPage

# Define server function  
server <- function(input, output) {
  
  output$plot <- renderPlot({
    ggplot(df, aes(x = .data[[input$c1]], y = .data[[input$c2]]),) +
      geom_point(color=input$PlotColor, size=as.numeric(input$Plotsize))+geom_smooth()+
      ggtitle(input$title1) + 
      
      xlab(unlist(description[input$c1])) + ylab(unlist(description[input$c2]))
    
  })
  ############
  output$text1 <- renderPrint({
    if (input$fun == 1){
      max(df[,(input$c3)])  
    }else if (input$fun == 2){
      mean(df[,(input$c3)]) 
    }else{
      min(df[,(input$c3)])}
  })
  ########
  output$table <- DT::renderDataTable(DT::datatable({
    data <- df  }))
  #######
}
shinyApp(ui = ui, server = server)