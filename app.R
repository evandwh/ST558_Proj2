library(tidyr)
library(readr)
library(dplyr)
library(gt)
library(ggplot2)
library(shiny)
library(DT)
library(bslib)

nfl_data <- read_csv("data/NFL_play_data.csv") |>
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


ui <- fluidPage(
  pageWithSidebar(
    
    #Title Panel
    headerPanel("NFL Play Data (2009-2016)"),
    
    #Sidebar
    sidebarPanel(
      
      #2nd tab
      conditionalPanel(
        condition = "input.tabs == 'pa_by_type'",
        
        selectInput(
          "team",
          "Select Team:",
          choices = sort(unique(nfl_data$HomeTeam))
        ),
        selectInput(
          "playtype",
          "Select Play Type:",
          c("Run", "Pass", "Field Goal", "Spike", "Punt", "QB Kneel", "Sack")
        ),
        selectInput(
          inputId = "season",
          label = "Select Season(s):",
          choices = sort(unique(nfl_data$Season)),
          selected = "2009",
          multiple = TRUE
        ),
        radioButtons("pa",
                     "Select One",
                     c("EPA", "WPA")
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
                  sort(unique(nfl_data$HomeTeam))
        ),
        selectInput(
          inputId = "season",
          label = "Select Season(s):",
          choices = sort(unique(nfl_data$Season)),
          selected = "2009",
          multiple = TRUE
        ),
        radioButtons("pa",
                   "Select One",
                   c("EPA","WPA")
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
          plotOutput("histogram")
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
    
    req(input$pa)
    req(filtered_data())
    
    bin_width <- (
      max(filtered_data()[[input$pa]], na.rm = TRUE) - 
      min(filtered_data()[[input$pa]], na.rm = TRUE)) / 10

    filtered_data() |>
      ggplot(aes(x = .data[[input$pa]])) +
      geom_histogram(
        binwidth = bin_width,
        fill = "steelblue",
        color = "black"
      ) +
      labs(
        title = paste(isolate(input$pa), "per", input$playtype, "for", input$team),
        x = input$pa,
        y = "Count"
      )
  })
  
  output$density_ridges <- renderPlot({
    req(input$pa)
    req(filtered_data())
    
    limits <- switch(
      input$pa,
      "EPA" = c(-8, 8),
      "WPA" = c(-0.3, 0.3),
      NULL
    )
    
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
    
    nfl_data |>
      filter(
        posteam == input$team,
        PlayType == input$playtype,
        Season %in% input$season
      )
  })
}

shinyApp(ui = ui, server = server)
