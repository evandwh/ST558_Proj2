library(tidyr)
library(readr)
library(dplyr)
library(gt)
library(ggplot2)
library(shiny)
library(DT)
library(bslib)
library(shinydashboard)

nfl <- read_csv("data/NFL_play_data.csv")


ui <- dashboardPage(
  dashboardHeader(title="NFL Play Data (2009-2016)"),
  
  dashboardSidebar(    
    sidebarMenu(
      menuItem("Play Type vs. Gain", tabName = "scatplot", icon = icon("archive")),
      menuItem("Play Type Breakdown", tabName = "breakdown", icon = icon("laptop"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "scatplot",
              titlePanel("Play Type vs. Gain"),
      ),
      tabItem(tabName = "breakdown",
              titlePanel("Play Type Breakdown"),
      )
    )
  )
)


server <- function(input, output, session){
  
  
}

shinyApp(ui = ui, server = server)
