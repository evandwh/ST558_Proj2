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
      
      #2nd tab - Histogram
      conditionalPanel(
        condition = "input.tabs == 'pa_by_type'",
        
        selectInput(
          "team",
          "Select Team:",
          choices = sort(unique(nfl_data$posteam))
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
          selected = "2009",
          multiple = TRUE
        ),
        actionButton(
          "apply",
          "Apply Filters"
        )
      ),
      
      #3rd tab
      conditionalPanel(
        condition = "input.tabs == 'pa_all'",
        selectInput("team",
                  "Select Team",
                  sort(unique(nfl_data$posteam))
        ),
        selectInput(
          inputId = "season",
          label = "Select Season(s):",
          choices = sort(unique(nfl_data$Season)),
          selected = "2009",
          multiple = TRUE
        ),
        actionButton(
          "apply",
          "Apply Filters"
        )
      ),
    ),
    #Main Area
    mainPanel(
      tabsetPanel(
        id = "tabs",
          
        #First Tab
        tabPanel(
          "About the Project",
          value = "about",
          h3("About the Data")
        ),
          
        #Second Tab
        tabPanel(
          "By Individual Play Type",
          value = "pa_by_type",
          plotOutput("histogram"),
          sliderInput(
            inputId = "binwidth",
            label = "Histogram Bin Width:",
            min = 1,
            max = 5,
            value = 1,
            step = 0.5
          )
        ),
          
        #Third Tab
        tabPanel(
          "All Play Types",
          value = "pa_all",
          plotOutput("density_ridges")
        )
      )
    )
  )
)


server <- function(input, output, session){
  
  output$histogram <- renderPlot({
    
    req(filtered_data())

    filtered_data() |>
      ggplot(aes(x = filtered_data()$yards_gained)) +
      geom_histogram(
        binwidth = input$binwidth,
        fill = "steelblue",
        color = "black"
      ) +
      labs(
        title = paste("Yards Gained per", input$playtype, "for", input$team),
        x = "Yards Gained",
        y = "Count"
      )
  })
  
  output$density_ridges <- renderPlot({
    req(input$pa)
    req(filtered_data())
    
    
    filtered_data() |>
      ggplot(aes(
        x = .data[[input$pa]],
        y = PlayType,
        fill = PlayType
      )) +
      geom_density_ridges(alpha = 0.7) +
      labs(
        title = paste("Distribution of", isolate(input$pa), "by Play Type"),
        x = input$pa,
        y = "Play Type"
      ) +
      theme(legend.position = "None") +
      coord_cartesian(xlim = limits)
  })
  
  
  filtered_data <- eventReactive(input$apply, {
    withProgress(
      message = "Filtering Data...",
      value = 1,
      nfl_data |>
        filter(
          posteam == input$team,
          PlayType == input$playtype,
          Season %in% input$season
      )
    )
  })
}

shinyApp(ui = ui, server = server)
