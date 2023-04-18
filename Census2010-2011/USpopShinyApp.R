#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Load in necessary libraries
library(shiny)
library(ggplot2)
library(rsconnect)
#reads in the url for the data
urltoread <- "http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
#Transfer to a data frame object
censusDatadf <- read.csv(url(urltoread))
#Data is very messy here.
head(censusDatadf)
summary(censusDatadf)

#Delete unnecessary rows and column
censusDatadf <- censusDatadf[,-6:-10]
censusDatadf <- censusDatadf[-60:-66,]
censusDatadf <- censusDatadf[-1:-8,]
#Change colomn names to be read more cleanly
colnames(censusDatadf) <- c("State", "April10census", "April10Base", "July10pop", "July11pop")
# Remove commas from values and gsub into numeric values.
censusDatadf$State <- gsub("\\.","", censusDatadf$State)
censusDatadf$April10census <- as.numeric(gsub(",", "", censusDatadf$April10census))
censusDatadf$April10Base <- as.numeric(gsub(",", "", censusDatadf$April10Base))
censusDatadf$July10pop <- as.numeric(gsub(",", "", censusDatadf$July10pop))
censusDatadf$July11pop <- as.numeric(gsub(",", "", censusDatadf$July11pop))

#view after cleaning. 
head(censusDatadf)
# this is where the UI of the shiny app is created.
# There are going to be four variables that you can choose from for the ggplot. 
# we have 4 options: State, July 2011 Population, Change in pop, and percent change in population.
#s1-3 and color are going to be drop down s1 is x-axis, s2 is yaxis, s3 is size (of the elements), and color of each element. 
#In this assignment we wanted to look at the July 2011 population but we could probably easily look at April from the data.
ui <- fluidPage(
  titlePanel("July 2011 Population Census Data: An Interactive Shiny App"),
  verticalLayout(
    s1 <- selectInput("variable", "x-axis:", 
                      c("State" = "censusDatadf$State", 
                        "July 2011 Population" =  "censusDatadf$July11pop",
                        "Change in Population" = "censusDatadf$popChange",
                        "Percent Change in Population" = "censusDatadf$percentChange")),
    s2 <- selectInput("variable1", "y-axis:", 
                      c("State" = "censusDatadf$State", 
                        "July 2011 Population" =  "censusDatadf$July11pop",
                        "Change in Population" = "censusDatadf$popChange",
                        "Percent Change in Population" = "censusDatadf$percentChange")),
    s3 <- selectInput("variable2", "Size:", 
                      c("State" = "censusDatadf$State", 
                        "July 2011 Population" =  "censusDatadf$July11pop",
                        "Change in Population" = "censusDatadf$popChange",
                        "Percent Change in Population" = "censusDatadf$percentChange")),
    color <- selectInput("variable3", "Color",
                         c("State" = "censusDatadf$State", 
                           "July 2011 Population" =  "censusDatadf$July11pop",
                           "Change in Population" = "censusDatadf$popChange",
                           "Percent Change in Population" = "censusDatadf$percentChange"))), 
  #This returns a plot to the dashboard.
  plotOutput("plot"))


# this fuction acts as the server a function that takes two argumanets input and output.
# Inside the function we remove the "state" of DC, change the state to lowercase, calculate the population change and change in percentage.

server <- function(input, output)
{
  
  censusDatadf <- censusDatadf[censusDatadf$State != "District of Columbia",]
  
  censusDatadf$State <- tolower(censusDatadf$State)
  censusDatadf$popChange <- (as.numeric(censusDatadf$July11pop) - as.numeric(censusDatadf$July10pop))
  censusDatadf$percentChange <- (as.numeric(censusDatadf$popChange) / as.numeric(censusDatadf$July10pop)) * 100
  
  
  output$plot <- renderPlot({
    ggplot(censusDatadf, aes_string(x = input$variable, y = input$variable1, colour = input$variable3, size = input$variable2)) + 
      geom_point() + 
      labs(x = input$variable, y =input$variable) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))})
  
}
#Using the shinyApp function we call our ui and server. 
shinyApp(ui, server)
