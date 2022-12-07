#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

chess_games = read.csv("data/chess_data.csv")
chess_games <- chess_games %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

chess_games_white <- select(chess_games,  -contains("black_name"))
names(chess_games_white) = gsub(pattern = "white_", 
                                replacement = "player_", 
                                x = names(chess_games_white))
names(chess_games_white) = gsub(pattern = "black_", 
                                replacement = "opponent_", 
                                x = names(chess_games_white))
# 0 - white win
# 1 - white draw
# 2 - white loss
# 3 - black loss
# 4 - black draw
# 5 - black win

chess_games_black <- select(chess_games,  -contains("white_name")) %>%
  mutate(result = result + 3)
names(chess_games_black) = gsub(pattern = "black_", 
                                replacement = "player_", 
                                x = names(chess_games_black))
names(chess_games_black) = gsub(pattern = "white_", 
                                replacement = "opponent_", 
                                x = names(chess_games_black))

chess_games_merged <- rbind(chess_games_white, chess_games_black) %>%
  mutate(result = cut(result, 
                      breaks = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
                      labels = c("White Win",
                                 "White Draw",
                                 "White Loss",
                                 "Black Loss",
                                 "Black Draw",
                                 "Black Win")))

top_50_gms <- tail(names(sort(table(chess_games_merged$name))), 50)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Chess Games Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Filter games by:"),
            dateRangeInput(
              "dates", 
              label = "Date",
              start = min(chess_games$date),
              end = max(chess_games$date),
              min = min(chess_games$date),
              max = max(chess_games$date),
            ),
            selectInput(
              "event", 
              label = "Event",
              choices = c("All Events", unique(chess_games$event_name)),
              selected = 1
            ),
            selectInput(
              "player", 
              label = "Player",
              choices = c("All Players", rev(top_50_gms)),
              selected = 1
            ),
            radioButtons(
              "color", 
              label = "Color", 
              choices = c("Both", "White", "Black"),
              selected = "Both"
            ),
            sliderInput(
              "player_ELO", 
              label = "Player ELO", 
              min = min(chess_games_merged$player_ELO), 
              max = max(chess_games_merged$player_ELO), 
              value = c(min(chess_games_merged$player_ELO),
                        max(chess_games_merged$player_ELO))
            ),
            sliderInput(
              "opponent_ELO", 
              label = "Opponent ELO", 
              min = min(chess_games_merged$opponent_ELO), 
              max = max(chess_games_merged$opponent_ELO), 
              value = c(min(chess_games_merged$opponent_ELO),
                        max(chess_games_merged$opponent_ELO))
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Plot", plotOutput("scatterPlot"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatterPlot <- renderPlot({
      chess_games_scatter <- chess_games_merged %>%
        filter(between(date, 
                       as.Date(input$dates[1]), 
                       as.Date(input$dates[2]))) %>%
        filter(between(player_ELO,
                       input$player_ELO[1],
                       input$player_ELO[2])) %>%
        filter(between(opponent_ELO,
                       input$opponent_ELO[1],
                       input$opponent_ELO[2]))
      
      chess_games_scatter %>%
        ggplot(aes(x = player_ELO, y = opponent_ELO)) +
        geom_point(
          aes(fill = result),
          color = "black",
          pch = 21,
          size = 0.2
        ) +
        scale_fill_manual(
          name = "Result",
          guide = "legend",
          limits = c("White Win",
                     "White Draw",
                     "White Loss",
                     "Black Loss",
                     "Black Draw",
                     "Black Win"),
          values = c("white",
                     "white",
                     "white",
                     "black",
                     "black",
                     "black")
        ) +
        labs(
          title = "Outcome vs Player Strength",
          x = "Player ELO",
          y = "Opponent ELO"
        ) +
        theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
