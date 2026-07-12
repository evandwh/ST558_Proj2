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

nfl_divisions <- list(
  
  AFC_North = c("BAL", "CIN", "CLE", "PIT"),
  AFC_South = c("HOU", "IND", "JAX", "TEN"),
  AFC_East  = c("BUF", "MIA", "NE" , "NYJ"),
  AFC_West  = c("DEN", "KC" , "LAC", "OAK"),
  NFC_North = c("CHI", "DET", "GB" , "MIN"),
  NFC_South = c("ATL", "CAR", "NO" , "TB" ),
  NFC_East  = c("DAL", "NYG", "PHI", "WAS"),
  NFC_West  = c("ARI", "LA" , "SF" , "SEA")
  )

ui <- fluidPage(
  pageWithSidebar(
    headerPanel("NFL Play Data (2009-2016)"),
    sidebarPanel(
      selectInput("team",
                "Select Team",
                sort(unique(nfl_data$HomeTeam))
      )
    ),
    card(
      plotOutput(outputId = "histogram")
    )
  )
)


server <- function(input, output, session){
  
  output$histogram <- renderPlot({
    
    req(input$team)
    
    nfl_data |>
      filter(posteam == input$team,
             !is.na(yards_gained)) |>
      ggplot(aes(x = yards_gained)) +
      geom_histogram(
        binwidth = 5,
        fill = "steelblue",
        color = "black"
      ) +
      labs(
        title = paste("Distribution of Yards Gained by", input$team),
        x = "Yards Gained",
        y = "Count"
      )
  })
  
}

shinyApp(ui = ui, server = server)
