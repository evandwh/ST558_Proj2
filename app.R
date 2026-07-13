
# Author: Evan Whitfield
# Purpose: ST 558 Project 2
# Last edited: 07-13-2026

# Still need to do about tab, and get all graphs and tables in the data exploration.
# Still need to make it look clean.

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
          value = "download",
          DT::dataTableOutput("data_table"),
          
          downloadButton(
            "download_data",
            "Download Data"
          )
        ),
          
        # Third Tab
        tabPanel(
          "Data Exploration",
          value = "explore",
          
          tabsetPanel(
            
            # -------------------
            # Histograms
            # -------------------
            tabPanel(
              "Histograms",
              
              plotOutput("histogram1"),
              
              sliderInput(
                "binwidth1",
                "Histogram 1 Bin Width",
                min = 1,
                max = 5,
                value = 1,
                step = 0.5
              ),
              
              hr(),
              
              plotOutput("histogram2"),
              
              sliderInput(
                "binwidth2",
                "Histogram 2 Bin Width",
                min = 1,
                max = 5,
                value = 1,
                step = 0.5
              )
            ),
            
            # -------------------
            # Categorical Summaries
            # -------------------
            tabPanel(
              "Categorical Summaries",
              
              h3("Play Type Counts", align = "center"),
              gt_output("playtype_table"),
              
              br(),
              
              h3("Play Type by Down", align = "center"),
              gt_output("playtype_down_table"),
              
              br(),
              
              h3("Play Type by Quarter", align = "center"),
              gt_output("playtype_qtr_table")
              
            ),
            
            # -------------------
            # Numeric Summaries
            # -------------------
            tabPanel(
              "Numeric Summaries",
              
              gt_output("numeric_summary")
            ),
            
            # -------------------
            # Other Graphs
            # -------------------
            tabPanel(
              "Other Graphs",
              
              plotOutput("scatterplot"),
              
              br(),
              
              plotOutput("boxplot"),
              
              br(),
              
              plotOutput("heatmap")
            )
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
            )
          )
        
        if (input$defteam_filter == "specific") {
          data <- data |>
            filter(DefensiveTeam %in% input$defteam)
        }
        
        data
      }
    )
  })
  
  output$data_table <- DT::renderDataTable({
    
    req(filtered_data())
    
    play_data <- filtered_data() |>
      filter(PlayType == input$playtype)
      
    play_data
  })
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste0(
        "NFL_filtered_data_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
      
    }
  )
  
  output$histogram1 <- renderPlot({
    
    req(app_state())
    
    state <- app_state()
    
    state$data |> 
      filter(PlayType == input$playtype) |>
      ggplot(aes(x = .data[[state$num_var1]])) +
      geom_histogram(
        binwidth = input$binwidth1,
        fill = "steelblue",
        color = "black"
      ) +
      labs(
        title = paste(state$num_var1, "per", input$playtype, "for", input$team),
        x = state$num_var1,
        y = "Count"
      )
  })
  
  output$histogram2 <- renderPlot({
    
    req(app_state())
    
    state <- app_state()
    
    state$data |>
      filter(PlayType == input$playtype) |>
      ggplot(aes(x = .data[[state$num_var2]])) +
      geom_histogram(
        binwidth = input$binwidth2,
        fill = "pink",
        color = "black"
      ) +
      labs(
        title = paste(state$num_var2, "per", input$playtype, "for", input$team),
        x = state$num_var2,
        y = "Count"
      )
  })
  
  output$playtype_table <- render_gt({
    
    req(app_state())
    
    state <- app_state()
    
    state$data |>
      count(PlayType, name = "Count") |>
      gt() |>
      cols_label(
        PlayType = "Play Type",
        Count = "Count"
      ) |>
      opt_row_striping()
    
  })
  
  output$playtype_down_table <- render_gt({
    
    req(app_state())
    
    state <- app_state()
    
    state$data |>
      count(PlayType, down) |>
      pivot_wider(
        names_from = down,
        values_from = n,
        values_fill = 0
      ) |>
      gt() |>
      cols_label(
        PlayType = "Play Type",
        `1` = "1st",
        `2` = "2nd",
        `3` = "3rd",
        `4` = "4th",
      ) |>
      opt_row_striping()
  })
  
  output$playtype_qtr_table <- render_gt({
    
    req(app_state())
    
    state <- app_state()
    
    state$data |>
      count(PlayType, qtr) |>
      pivot_wider(
        names_from = qtr,
        values_from = n,
        values_fill = 0
      ) |>
      gt() |>
      cols_label(
        PlayType = "Play Type",
        `1st` = "1st",
        `2nd` = "2nd",
        `3rd` = "3rd",
        `4th` = "4th",
        `OT` = "Overtime"
      ) |>
      opt_row_striping()
  })
  
  app_state <- eventReactive(input$apply, {
    list(
      data = filtered_data(),
      num_var1 = input$num_var1,
      num_var2 = input$num_var2
    )
  })

}

shinyApp(ui = ui, server = server)
