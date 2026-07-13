library(tidyr)
library(readr)
library(dplyr)
library(gt)
library(ggplot2)
library(shiny)
library(DT)
library(bslib)

all_nfl_data <- read_csv("data/NFL_play_data.csv") |>
  rename(yards_gained = Yards.Gained) |>
  #Need to change the name of a couple of teams to make sure that there are only 32 teams listed.
  mutate(
    across(
      c(posteam, DefensiveTeam, HomeTeam, AwayTeam),
      ~ recode(
        .x,
        "STL" = "LA",
        "JAC" = "JAX"
      )
    )
  ) |>
  mutate(
    qtr = factor(
      qtr,
      levels = c(1, 2, 3, 4, 5),
      labels = c("1st", "2nd", "3rd", "4th", "OT")
    )
  )

nfl_data <- all_nfl_data |>
  select(PlayType, posteam, yards_gained, down, qtr, 
         ydstogo, ScoreDiff, Season, PassOutcome, DefensiveTeam) |>
  filter(!PlayType %in% c("End of Game", 
                          "Extra Point", 
                          "Half End", 
                          "Kickoff", 
                          "Quarter End",
                          "Two Minute Warning",
                          "Timeout",
                          "No Play"))

ui <- fluidPage(
  pageWithSidebar(
    
    #Title Panel
    headerPanel("NFL Play Data (2009-2016)"),
    
    #Sidebar
    sidebarPanel(
        
      selectInput(
        "team",
        "Select Team:",
        choices = sort(unique(nfl_data$posteam)),
        selected = "CAR",
        multiple = TRUE
      ),
      selectInput(
        "playtype",
        "Select Play Type:",
        c("Run", "Pass")
      ),
      selectInput(
        inputId = "season",
        label = "Select Season(s):",
        choices = sort(unique(nfl_data$Season)),
        selected = "2015",
        multiple = TRUE
      ),
      selectInput(
        "num_var1",
        "Numeric Variable 1:",
        choices = c(
          "Yards Gained" = "yards_gained",
          "Yards to Go" = "ydstogo",
          "Score Differential" = "ScoreDiff"
        )
      ),
      uiOutput("num_slider1"),
      selectInput(
        "num_var2",
        "Numeric Variable 2:",
        choices = c(
          "Yards Gained" = "yards_gained",
          "Yards to Go" = "ydstogo",
          "Score Differential" = "ScoreDiff"
        )
      ),
      uiOutput("num_slider2"),
      selectInput(
        "down",
        "Down:",
        choices = c("1", "2", "3", "4"),
        selected = c("1", "2", "3", "4"),
        multiple = TRUE
      ),
      selectInput(
        "qtr",
        "Quarter",
        choices = c("1st", "2nd", "3rd", "4th", "OT"),
        selected = c("1st", "2nd", "3rd", "4th", "OT"),
        multiple = TRUE
      ),
      radioButtons(
        "defteam_filter",
        "Opponent Filter:",
        choices = c(
          "Whole Season" = "all",
          "Specific Team(s)" = "specific"
        ),
        selected = "all"
      ),
      conditionalPanel(
        condition = "input.defteam_filter == 'specific'",
        selectInput(
          "defteam",
          "Select Defending Team(s):",
          choices = sort(unique(nfl_data$DefensiveTeam)),
          selected = "ATL",
          multiple = TRUE
        )
      ),
      actionButton(
        "apply",
        "Apply Filters"
      )
    ),
    
    #Main Area
    mainPanel(
      tabsetPanel(
        id = "tabs",
          
        #First Tab
        tabPanel(
          "About the Project",
          value = "about",
          h2("About the App"),
          h5("More info about the data can be found at 
             https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016/data")
        ),
          
        #Second Tab
        tabPanel(
          "Data Download",
          value = "download"
        ),
          
        #Third Tab
        tabPanel(
          "Data Exploration",
          value = "explore",
          plotOutput("histogram"),
          sliderInput(
            inputId = "binwidth",
            label = "Histogram Bin Width:",
            min = 1,
            max = 5,
            value = 1,
            step = 0.5
          )
        )
      )
    )
  )
)


server <- function(input, output, session){
  
  output$num_slider1 <- renderUI({
    
    req(input$num_var1)
    
    sliderInput(
      "num_range1",
      label = paste("Filter", input$num_var1),
      min = floor(min(nfl_data[[input$num_var1]], na.rm = TRUE)),
      max = ceiling(max(nfl_data[[input$num_var1]], na.rm = TRUE)),
      value = c(
        floor(min(nfl_data[[input$num_var1]], na.rm = TRUE)),
        ceiling(max(nfl_data[[input$num_var1]], na.rm = TRUE))
      )
    )
  })
  
  output$num_slider2 <- renderUI({
    
    req(input$num_var2)
    sliderInput(
      "num_range2",
      label = paste("Filter", input$num_var2),
      min = floor(min(nfl_data[[input$num_var2]], na.rm = TRUE)),
      max = ceiling(max(nfl_data[[input$num_var2]], na.rm = TRUE)),
      value = c(
        floor(min(nfl_data[[input$num_var2]], na.rm = TRUE)),
        ceiling(max(nfl_data[[input$num_var2]], na.rm = TRUE))
      )
    )
  })
  
  filtered_data <- eventReactive(input$apply, {
    withProgress(
      message = "Filtering Data...",
      value = 1,
      {
        data <- nfl_data |>
          filter(
            posteam %in% input$team,
            PlayType == input$playtype,
            Season %in% input$season,
            qtr %in% input$qtr,
            down %in% input$down,
            between(
              .data[[input$num_var1]],
              input$num_range1[1],
              input$num_range1[2]
              ),
            between(
              .data[[input$num_var2]],
              input$num_range2[1],
              input$num_range2[2]
            ),
          )
        
        if (input$defteam_filter == "specific") {
          data <- data |>
            filter(DefensiveTeam %in% input$defteam)
        }
        
        data
      }
    )
  })
  
  output$histogram <- renderPlot({
    
    req(filtered_data())

    filtered_data() |>
      ggplot(aes(x = .data[[input$num_var1]])) +
      geom_histogram(
        binwidth = input$binwidth,
        fill = "steelblue",
        color = "black"
      ) +
      labs(
        title = paste(input$num_var1, "per", input$playtype, "for", input$team),
        x = "Yards Gained",
        y = "Count"
      )
  })
  

}

shinyApp(ui = ui, server = server)
