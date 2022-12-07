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
library(gghighlight)

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

chess_games_merged <- chess_games_merged[order(chess_games_merged$date),]

top_50_gms <- tail(names(sort(table(chess_games_merged$player_name))), 50)

chess_games_merged <- chess_games_merged %>%
  filter(player_name %in% top_50_gms)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Chess Games Analysis of 50 Top Players"),
  tabsetPanel(
    tabPanel("Outcome vs Player Strength", 
             sidebarLayout(
               sidebarPanel(
                 h3("Filter games by:"),
                 dateRangeInput("scatter_dates", 
                                label = "Date",
                                start = min(chess_games$date),
                                end = max(chess_games$date),
                                min = min(chess_games$date),
                                max = max(chess_games$date),
                 ),
                 selectInput("scatter_event", 
                             label = "Event",
                             choices = c("All Events", 
                                         unique(chess_games$event_name)),
                             selected = 1
                 ),
                 selectInput("scatter_player", 
                             label = "Player",
                             choices = c("All Players", 
                                         rev(top_50_gms)),
                             selected = 1
                 ),
                 radioButtons("scatter_color", 
                              label = "Color", 
                              choices = c("Both", "White", "Black"),
                              selected = "Both"
                 ),
                 radioButtons("scatter_result", 
                              label = "Result", 
                              choices = c("All", "Win", "Draw", "Loss"),
                              selected = "All"
                 ),
                 sliderInput("scatter_player_ELO", 
                             label = "Player ELO", 
                             min = min(chess_games_merged$player_ELO), 
                             max = max(chess_games_merged$player_ELO), 
                             value = c(min(chess_games_merged$player_ELO),
                                       max(chess_games_merged$player_ELO))
                 ),
                 sliderInput("scatter_opponent_ELO", 
                             label = "Opponent ELO", 
                             min = min(chess_games_merged$opponent_ELO), 
                             max = max(chess_games_merged$opponent_ELO), 
                             value = c(min(chess_games_merged$opponent_ELO),
                                       max(chess_games_merged$opponent_ELO))
                 )
               ),
               mainPanel(plotOutput("scatterPlot"))
             )
    ),
    tabPanel("Player ELO over time",
             sidebarLayout(
               sidebarPanel(
                 h3("Filter games by:"),
                 dateRangeInput("rating_line_dates", 
                                label = "Date",
                                start = min(chess_games$date),
                                end = max(chess_games$date),
                                min = min(chess_games$date),
                                max = max(chess_games$date),
                 ),
                 selectInput("rating_line_player", 
                             label = "Highlight Player",
                             choices = c("No Player Selected", rev(top_50_gms)),
                             selected = 1
                 ),
                 sliderInput("rating_line_player_ELO", 
                             label = "Player ELO", 
                             min = min(chess_games_merged$player_ELO), 
                             max = max(chess_games_merged$player_ELO), 
                             value = c(min(chess_games_merged$player_ELO),
                                       max(chess_games_merged$player_ELO))
                 )
               ),
               mainPanel(plotOutput("ratingLinePlot"))
             )
    ),
    tabPanel("Average centipawn loss vs ELO",
             sidebarLayout(
               sidebarPanel(
                 h3("Filter games by:"),
                 dateRangeInput("acpl_line_dates", 
                                label = "Date",
                                start = min(chess_games$date),
                                end = max(chess_games$date),
                                min = min(chess_games$date),
                                max = max(chess_games$date),
                 ),
                 selectInput("acpl_line_player", 
                             label = "Highlight Player",
                             choices = c("No Player Selected", rev(top_50_gms)),
                             selected = 1
                 ),
                 sliderInput("acpl_line_player_ELO", 
                             label = "Player ELO", 
                             min = min(chess_games_merged$player_ELO), 
                             max = max(chess_games_merged$player_ELO), 
                             value = c(min(chess_games_merged$player_ELO),
                                       max(chess_games_merged$player_ELO))
                 )
               ),
               mainPanel(plotOutput("acplLinePlot"))
             )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$scatterPlot <- renderPlot({
      chess_games_scatter <- chess_games_merged %>%
        filter(between(date, 
                       as.Date(input$scatter_dates[1]), 
                       as.Date(input$scatter_dates[2]))) %>%
        filter(between(player_ELO,
                       input$scatter_player_ELO[1],
                       input$scatter_player_ELO[2])) %>%
        filter(between(opponent_ELO,
                       input$scatter_opponent_ELO[1],
                       input$scatter_opponent_ELO[2]))
      
      if (input$scatter_event != "All Events") {
        chess_games_scatter <- chess_games_scatter %>%
          filter(event_name == input$scatter_event)
      }
      
      if (input$scatter_player != "All Players") {
        chess_games_scatter <- chess_games_scatter %>%
          filter(player_name == input$scatter_player)
      }
      
      if (input$scatter_color != "Both"){
        chess_games_scatter <- chess_games_scatter %>%
          filter(grepl(input$scatter_color, result))
      }
      
      if (input$scatter_result != "All"){
        chess_games_scatter <- chess_games_scatter %>%
          filter(grepl(input$scatter_result, result))
      }
      
      chess_games_scatter %>%
        ggplot(aes(x = player_ELO, y = opponent_ELO)) +
        geom_point(
          aes(color = result, shape = result),
          size = 2,
          alpha = 0.8
        ) +
        scale_shape_manual(
          name = "Result",
          guide = "legend",
          limits = c("White Win",
                     "White Draw",
                     "White Loss",
                     "Black Win",
                     "Black Draw",
                     "Black Loss"),
          values = c(15, 
                     17, 
                     19, 
                     15, 
                     17, 
                     19) 
        ) +
        scale_color_manual(
          name = "Result",
          guide = "legend",
          limits = c("White Win",
                     "White Draw",
                     "White Loss",
                     "Black Win",
                     "Black Draw",
                     "Black Loss"),
          values = c("#e3c059",
                     "#e3c059",
                     "#e3c059",
                     "black",
                     "black",
                     "black")
        ) +
        labs(
          x = "Player ELO",
          y = "Opponent ELO"
        ) +
        theme_minimal()
    })
    
    output$ratingLinePlot <- renderPlot({
      chess_games_rating_line <- chess_games_merged %>%
        filter(between(date, 
                       as.Date(input$rating_line_dates[1]), 
                       as.Date(input$rating_line_dates[2]))) %>%
        filter(between(player_ELO,
                       input$rating_line_player_ELO[1],
                       input$rating_line_player_ELO[2]))
      
      chess_games_rating_line %>%
        ggplot(aes(x = date, y = player_ELO)) +
        geom_line(
          aes(color = player_name),
          show.legend = FALSE
        ) +
        gghighlight(player_name == input$rating_line_player) +
        labs(
          x = "Date",
          y = "Player ELO"
        ) +
        theme_minimal()
    })
    
    output$acplLinePlot <- renderPlot({
      chess_games_acpl_line <- chess_games_merged %>%
        filter(between(date, 
                       as.Date(input$acpl_line_dates[1]), 
                       as.Date(input$acpl_line_dates[2]))) %>%
        filter(between(player_ELO,
                       input$acpl_line_player_ELO[1],
                       input$acpl_line_player_ELO[2]))
      
      
      chess_games_acpl_line <- chess_games_acpl_line %>%
        mutate(player_ELO = floor(player_ELO/20) * 20) %>%
        group_by(player_ELO, player_name) %>%
        summarize(player_avg_CP_loss = mean(player_avg_CP_loss))
      
      chess_games_acpl_line %>%
        ggplot(aes(x = player_ELO, 
                   y = player_avg_CP_loss)) +
        geom_line(
          aes(color = player_name),
          show.legend = FALSE
        ) +
        gghighlight(player_name == input$acpl_line_player) +
        labs(
          x = "Player ELO",
          y = "Average centipawn loss"
        ) +
        theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
