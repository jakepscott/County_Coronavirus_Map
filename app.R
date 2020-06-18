# Loading Libraries--------------------------------------------------------
library(shiny)
library(tidyverse)
library(lubridate)
library(geofacet)
library(scales)
library(zoo)
library(ggtext) 
library(ggthemes)
library(shinythemes)
library(gganimate)
library(sf)
library(tidycensus)
library(plotly)

source("County_Level_Function.R")

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  ##Setting Theme
  theme = shinytheme("journal"), 
  # Application title
  titlePanel("Coronavirus by County"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("statename","State", choices=state.name),
        uiOutput("countySelection"),
        selectInput("measure", "Measure", 
                    choices=c("New Cases", 
                              "New Deaths",
                              "New Cases Per 100k",
                              "New Deaths Per 100k"),
                    selected = "New Cases"),
        sliderInput("RollingAverage", 
                    label="Window for rolling average",
                    min=1,
                    max=14,
                    value=7,
                    step=1)
      ),
      
      mainPanel(
         plotlyOutput("plot",height=600)
      )
   )
)



# Server ------------------------------------------------------------------
server <- function(input, output) {

# Set Up Code -------------------------------------------------------------
  ##Pop Data
  county_pop_clean <- read_rds("data/county_pop_clean.RDS")
  state_pop_clean <- read_rds("data/state_pop_clean.RDS")
  
  ##Raw Data
  State_Names <- read_rds("data/State_Names.RDS")
  US_Data_Raw <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
  
  #Cleaned Data
  US_Data <- left_join(US_Data_Raw,State_Names, by=c("state"))
  US_Data <- left_join(US_Data,state_pop_clean,by="state")
  US_Data <- left_join(US_Data,county_pop_clean, by=c("state","county")) %>%
    rename("County"=county,"State"=state, "Date"=date, "Cases"=cases,"Deaths"=deaths)
  US_Data <- US_Data %>% filter(State %in% State_Names$state | State=="District of Columbia") 

# Second Selection --------------------------------------------------------

  
  output$countySelection <- renderUI({
    counties <- US_Data %>% filter(State==input$statename & County!="Unknown") %>% arrange(County)
    selectInput("county", "County", choices = unique(counties$County))
  })
  
  output$plot <- renderPlotly({
    req(input$county!="")
    req(input$statename!="")
    ggplotly(county_graph(state = input$statename, county = input$county,
                 measure = input$measure, rollmean = input$RollingAverage),
             tooltip="text")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

