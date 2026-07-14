
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
library(ggbeeswarm)

all_nfl_data <- read_csv("data/NFL_play_data.csv") |>
  rename(yards_gained = Yards.Gained) |>
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
    
    # -------------------
    # Title
    # -------------------
    headerPanel("NFL Play Data (2009-2016)"),
    
    # -------------------
    # Sidebar
    # -------------------
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
        ),
        selected = "yards_gained"
      ),
      uiOutput("num_slider1"),
      selectInput(
        "num_var2",
        "Numeric Variable 2:",
        choices = c(
          "Yards Gained" = "yards_gained",
          "Yards to Go" = "ydstogo",
          "Score Differential" = "ScoreDiff"
        ),
        selected = "ydstogo"
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
    
    # -------------------
    # Main Panel
    # -------------------
    mainPanel(
      tabsetPanel(
        id = "tabs",
          
        # -------------------
        # About Section
        # -------------------
        tabPanel(
          "About the Project",
          value = "about",
          h2("NFL Play-by-Play Data Explorer"),
          
          p(
            "This interactive Shiny application allows users to explore NFL play-by-play data ",
            "from the 2009 through 2016 regular seasons. Users can filter the data by season, ",
            "offensive team, defensive team, play type (just run and pass), quarter, down, and 
            additional variables to investigate offensive tendencies and game situations."
          ),
          
          p(
            "The application provides interactive visualizations, categorical summaries, ",
            "numerical summaries, and downloadable filtered datasets to help users analyze ",
            "NFL play-calling behavior and play outcomes."
          ),
          
          p(
            "More information about the data and can be found at the website below."
          ),
          
          p(
            tags$a(
              href = "https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016/data",
              "View the dataset on Kaggle",
              target = "_blank"
            )
          ),
          
          p(
            "I would like to add much more functionality and change the look of the App in the",
            "future. Future plans include including all play types to be filtered. EPA and WPA",
            "were planned to be the main focus of this project, but were scrapped due to some",
            "of the project requirements. I would like to add further exploration into those variables."
          ),
          
          br(),
          
          p(
            em("Created for project 2 for ST 558.")
          )
        ),
        
        # -------------------
        # Data Download
        # -------------------
        tabPanel(
          "Data Download",
          value = "download",
          DT::dataTableOutput("data_table"),
          
          downloadButton(
            "download_data",
            "Download Data"
          )
        ),
          
        # -------------------
        # Data Exploration
        # -------------------
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
              h3("Summary by Play Type", center = "align"),
              gt_output("yds_sum_table")
            ),
            
            # -------------------
            # Other Graphs
            # -------------------
            
            tabPanel(
              "Other Graphs",
              
              h3("Scatterplot of Yards on different plays", align = "center"),
              
              plotOutput("scatterplot"),
              
              h3("Boxplot Comparison of Play Types", align = "center"),
              
              plotOutput("boxplot"),
              
              h3("Play Type Distribution by Quarter", align = "center"),
              
              plotOutput("playtype_quarter_bar"),
              
              h3("Distribution Plot", align = "center"),
              
              plotOutput("yards_quasirandom")
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
          ) |>
          droplevels()
        
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
      mutate(
        down = factor(
          down,
          levels = c(1, 2, 3, 4)
        )
      ) |>
      pivot_wider(
        names_from = down,
        values_from = n,
        values_fill = 0
      ) |>
      select(PlayType, '1', '2','3', '4') |>
      gt() |>
      cols_label(
        PlayType = "Play Type",
        `1` = "1st",
        `2` = "2nd",
        `3` = "3rd",
        `4` = "4th"
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
      select(PlayType, any_of(c('1st', '2nd','3rd', '4th', 'OT'))) |>
      gt() |>
      opt_row_striping()
  })
  
  output$yds_sum_table <- render_gt({
    
    req(app_state())
    
    state <- app_state()
    
    state$data |>
      group_by(PlayType) |>
      summarize(
        Mean = mean(.data[[state$num_var1]], na.rm = TRUE),
        Median = median(.data[[state$num_var1]], na.rm = TRUE),
        SD = sd(.data[[state$num_var1]], na.rm = TRUE),
        Minimum = min(.data[[state$num_var1]], na.rm = TRUE),
        Maximum = max(.data[[state$num_var1]], na.rm = TRUE),
        Count = sum(!is.na(.data[[state$num_var1]])),
        .groups = "drop"
      ) |>
      gt() |>
      fmt_number(
        columns = c(Mean, Median, SD, Minimum, Maximum),
        decimals = 4
      ) |>
      cols_label(
        PlayType = "Play Type",
        Mean = "Mean",
        Median = "Median",
        SD = "Standard Deviation",
        Minimum = "Minimum",
        Maximum = "Maximum",
        Count = "Count"
      ) |>
      tab_header(
        title = paste(
          "Summary of",
          state$num_var1,
          "by Play Type"
        )
      )
    
  })
  
  output$yards_quasirandom <- renderPlot({
    
    req(app_state())
    
    state <- app_state()
    
    state$data |>
      ggplot(
        aes(
          x = factor(qtr),
          y = .data[[state$num_var1]],
          color = PlayType
        )
      ) +
      geom_quasirandom(
        alpha = 0.8,
        width = 0.25
      ) +
      labs(
        title = paste(
          "Distribution of",
          state$num_var1,
          "by Quarter"
        ),
        x = "Quarter",
        y = state$num_var1,
        color = "Play Type"
      )
  })

  app_state <- eventReactive(input$apply, {
    list(
      data = filtered_data(),
      num_var1 = input$num_var1,
      num_var2 = input$num_var2
    )
  })

  output$scatterplot <- renderPlot({
    
    req(app_state())
    
    state <- app_state()
    
    state$data |>
      ggplot(
        aes(
          x = .data[[state$num_var1]],
          y = .data[[state$num_var2]],
          color = PlayType
        )
      ) +
      geom_point(
        alpha = 0.6
      ) +
      labs(
        title = paste(
          state$num_var2,
          "vs.",
          state$num_var1
        ),
        x = state$num_var1,
        y = state$num_var2,
        color = "Play Type"
      )
    
  })
  
  output$boxplot <- renderPlot({
    
    req(app_state())
    
    state <- app_state()
    
    state$data |>
      ggplot(
        aes(PlayType,
            y = .data[[state$num_var1]],
            fill = PlayType)) +
      geom_boxplot() +
      labs(
        title = paste(state$num_var1, "When Play Type Called"),
        x = "Play Type",
        y = state$num_var1
      ) + 
      theme(legend.position = "None",
            plot.title = element_text(hjust = 0.5)
      )
  })
  
  
  output$playtype_quarter_bar <- renderPlot({
    
    req(app_state())
    
    state <- app_state()
    
    play_type_qtr_counts <- state$data |>
      count(qtr, PlayType)
    
    ggplot(
      play_type_qtr_counts |> filter(!is.na(qtr)),
      aes(
        x = reorder(PlayType, -n),
        y = n,
        fill = PlayType
      )
    ) +
      geom_col() +
      facet_wrap(~qtr) +
      labs(
        title = "Play Type Distribution by Quarter",
        x = "Play Type",
        y = "Number of Plays"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)
      )
    
  })
}

shinyApp(ui = ui, server = server)
