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

# read and format data
chess_games = read.csv("data/chess_data.csv")
chess_games <- chess_games %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

# stack black and white column to filter by player
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
# rename columns to stack datasets
names(chess_games_black) = gsub(pattern = "black_", 
                                replacement = "player_", 
                                x = names(chess_games_black))
names(chess_games_black) = gsub(pattern = "white_", 
                                replacement = "opponent_", 
                                x = names(chess_games_black))

# change result from continuous to categorical
chess_games_merged <- rbind(chess_games_white, chess_games_black) %>%
  mutate(result = cut(result, 
                      breaks = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
                      labels = c("White Win",
                                 "White Draw",
                                 "White Loss",
                                 "Black Loss",
                                 "Black Draw",
                                 "Black Win")))

# sort stacked dataframe by date
chess_games_merged <- chess_games_merged[order(chess_games_merged$date),]

# retrieve 50 players with most games in database
top_50_gms <- tail(names(sort(table(chess_games_merged$player_name))), 50)

chess_games_merged <- chess_games_merged %>%
  filter(player_name %in% top_50_gms)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Chess Games Analysis of 50 Top Players"),
  p("This is an analysis of over 12000 games from 50 top chess players and 
    grandmasters (GMs). It seeks to compare the",
    a("ELO", href = "https://www.chess.com/terms/elo-rating-chess"),
    "and",
    a("average centipawn loss (ACPL)", href = "https://lichess.org/faq#acpl"),
    "of different players across different events, results, dates, and ratings 
    to discover trends that may be linked to cheating. This is in response to
    claims levied by World Chess Champion Magnus Carlsen and the popular 
    chess website Chess.com, among others, that GM Hans Niemann cheated in
    over-the-board events as well as more online events than he previously 
    admitted to.
    The dataset used is adapted from",
    a("Kaggle.", href = "https://www.kaggle.com/datasets/tompaulat/10000-chess-games-with-centipawn-loss"),
    "The source code and adapted dataset is available through",
    a("GitHub.", href = "https://github.com/zachjhuang/chess-analysis")),
  tabsetPanel(
    tabPanel("Outcome vs player strength", 
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
                 ),
                 sliderInput("scatter_player_avg_CP_loss", 
                             label = "Player Average Centipawn Loss", 
                             min = min(chess_games_merged$player_avg_CP_loss), 
                             max = max(chess_games_merged$player_avg_CP_loss), 
                             value = c(min(chess_games_merged$player_avg_CP_loss),
                                       max(chess_games_merged$player_avg_CP_loss))
                 ),
                 sliderInput("scatter_opponent_avg_CP_loss", 
                             label = "Opponent Average Centipawn Loss", 
                             min = min(chess_games_merged$opponent_avg_CP_loss), 
                             max = max(chess_games_merged$opponent_avg_CP_loss), 
                             value = c(min(chess_games_merged$opponent_avg_CP_loss),
                                       max(chess_games_merged$opponent_avg_CP_loss))
                 )
               ),
               mainPanel(plotOutput("scatterPlot"),
                         h3("Select y-axis variable:"),
                         br(),
                         radioButtons("scatter_y",
                                      label = NULL,
                                      choices = c(
                                        "Opponent ELO",
                                        "Player Average Centipawn Loss",
                                        "Opponent Average Centipawn Loss"),
                                      selected = "Opponent ELO"
                         ),
               )
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
    ),
    tabPanel("Insights",
             br(),
             p("If it was possible to concretely determine whether GM Hans 
             Niemann cheated from data analysis, there wouldn't be a controversy 
             in the first place. Filtering recently played games by player, 
             result, or color and 
             comparing his results to other top grandmasters reveals nothing 
             out of the ordinary. For example, his ACPL in wins
             follows a similar pattern to the other players (Outcome vs Player 
             Strength), he doesn't seem to be scoring wins over higher-rated 
             opponents with either color more than any other top GM (Outcome vs player strength), 
             and his ACPL decreases with increases in ELO similar to other 
               GMs (Average centipawn loss vs ELO)."),
             br(),
             p("The trend that stands out the most is his steep climb in ELO 
             rating over time. But by filtering for games past 2010 (Player ELO 
             over time), it's clear that there are other young GMs such as 
             Praggnanandhaa, Erigaisi, and Firouzja with similar trajectories 
             that have not been so boldly accused of cheating."
             )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$scatterPlot <- renderPlot({
      # change y-axis based on radio buttons
      scatter_y = switch(input$scatter_y,
                         "Opponent ELO" = "opponent_ELO", 
                         "Player Average Centipawn Loss" = "player_avg_CP_loss",
                         "Opponent Average Centipawn Loss" = "opponent_avg_CP_loss")
      
      chess_games_scatter <- chess_games_merged %>%
        # filter data based on widget inputs
        filter(between(date, 
                       as.Date(input$scatter_dates[1]), 
                       as.Date(input$scatter_dates[2]))) %>%
        filter(between(player_ELO,
                       input$scatter_player_ELO[1],
                       input$scatter_player_ELO[2])) %>%
        filter(between(opponent_ELO,
                       input$scatter_opponent_ELO[1],
                       input$scatter_opponent_ELO[2])) %>%
        filter(between(player_avg_CP_loss,
                       input$scatter_player_avg_CP_loss[1],
                       input$scatter_player_avg_CP_loss[2])) %>%
        filter(between(opponent_avg_CP_loss,
                       input$scatter_opponent_avg_CP_loss[1],
                       input$scatter_opponent_avg_CP_loss[2])) 
      
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
        ggplot(aes_string(x = "player_ELO", 
                          y = scatter_y)
        ) +
        geom_point(
          aes(color = result, shape = result),
          size = 2,
          alpha = 0.8
        ) +
        geom_abline(intercept = 0, slope = 1) +
        scale_shape_manual(
          name = "Player Result",
          guide = "legend",
          limits = c("White Win",
                     "White Draw",
                     "White Loss",
                     "Black Win",
                     "Black Draw",
                     "Black Loss"),
          values = c(2, 
                     1, 
                     4, 
                     2, 
                     1, 
                     4) 
        ) +
        scale_color_manual(
          name = "Player Result",
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
          title = "Above the line, opponent rated higher; below the line, player rated higher",
          x = "Player ELO",
          y = input$scatter_y
        ) +
        theme_minimal()
    })
    
    output$ratingLinePlot <- renderPlot({
      chess_games_rating_line <- chess_games_merged %>%
        # filter data based on widget inputs
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
      # filter data based on widget inputs
      chess_games_acpl_line <- chess_games_merged %>%
        filter(between(player_ELO,
                       input$acpl_line_player_ELO[1],
                       input$acpl_line_player_ELO[2]))
      
      # bin ACPL by ELO and find mean ACPL for each bin
      chess_games_acpl_line <- chess_games_acpl_line %>%
        mutate(player_ELO = floor(player_ELO/50) * 50) %>%
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
