library(tidyverse)

rm(list = ls())

load("FINALmodel.RData")
load("std_params.RData")


# UI ----------------------------------------------------------------------

ui <- fluidPage(theme = "stylesheet.css",
  titlePanel(h1("The Machine", align = "center")),
  
  fluidRow(

# Top ---------------------------------------------------------------------
    column(12, align = "center",
           h2("Offered Bedding Odds", align = "center"),
           dataTableOutput("winner"),
           div(id = "container1", textOutput("odds0")),
           div(id = "container2", ":"),
           div(id = "container3", textOutput("odds1")),
           textOutput("eta")),
    column(12, align = "center",
           column(2
           ),

# Player 1 ----------------------------------------------------------------
           column(4,
                  h2("Player 1", align = "center"),
                  # numericInput("name0",
                  #              label = "Name of Player 1",
                  #              value = 1),
                  sliderInput("age0",
                              label = "Age of Player 1",
                              min = 15, max = 45, value = 25, step = 1),
                  sliderInput("height0",
                               label = "Height of Player 1",
                              min = 150, max = 225, value = 180, step = 1),
                  sliderInput("titles0",
                               label = "Titles of Player 1",
                              min = 0, max = 120, value = 5, step = 1),
                  sliderInput("rank0",
                               label = "Rank of Player 1",
                              min = 1, max = 2058, value = 100, step = 1),
                  sliderInput("form_1_0",
                               label = "Last game won Player 1",
                              min = 0, max = 1, value = 0, step = 1),
                  sliderInput("form_5_0",
                               label = "Last 5 games won Player 1",
                              min = 0, max = 5, value = 0, step = 1),
                  sliderInput("form_10_0",
                               label = "Last 10 games won Player 1",
                              min = 0, max = 10, value = 0, step = 1),
                  sliderInput("h2h0",
                               label = "Encounters won by Player 1",
                              min = 0, max = 60, value = 30, step = 1),
                  sliderInput("condition_wins0",
                               label = "Court condition win% of Player 1",
                              min = 0, max = 100, value = 50, step = 1),
                  # sliderInput("fatigue_1_0",
                  #              label = "Duration last game Player 1",
                  #               min = 1, max = 100, value = 25),
                  # sliderInput("fatigue_2_0",
                  #              label = "Duration penultimate game Player 1",
                  #               min = 1, max = 100, value = 25),
                  sliderInput("home_game0",
                               label = "Home game for Player 1",
                              min = 0, max = 1, value = 0, step = 1)
                  ),

# Player 2 ----------------------------------------------------------------
           column(4,
                  h2("Player 2", align = "center"),
                  # numericInput("name1",
                  #              label = "Name of Player 2",
                  #              value = 1),
                  sliderInput("age1",
                               label = "Age of Player 2",
                              min = 15, max = 45, value = 25, step = 1),
                  sliderInput("height1",
                               label = "Height of Player 2",
                              min = 150, max = 225, value = 180, step = 1),
                  sliderInput("titles1",
                               label = "Titles of Player 2",
                              min = 0, max = 120, value = 5, step = 1),
                  sliderInput("rank1",
                               label = "Rank of Player 2",
                              min = 1, max = 2058, value = 100, step = 1),
                  sliderInput("form_1_1",
                               label = "Last game won Player 2",
                              min = 0, max = 1, value = 0, step = 1),
                  sliderInput("form_5_1",
                               label = "Last 5 games won Player 2",
                              min = 0, max = 5, value = 0, step = 1),
                  sliderInput("form_10_1",
                               label = "Last 10 games won Player 2",
                              min = 0, max = 10, value = 0, step = 1),
                  sliderInput("h2h1",
                               label = "Encounters won by Player 2",
                              min = 0, max = 60, value = 30, step = 1),
                  sliderInput("condition_wins1",
                               label = "Court condition win% of Player 2",
                              min = 0, max = 100, value = 50, step = 1),
                  # sliderInput("fatigue_1_1",
                  #              label = "Duration last game Player 2",
                  #               min = 1, max = 100, value = 25),
                  # sliderInput("fatigue_2_1",
                  #              label = "Duration penultimate game Player 2",
                  #               min = 1, max = 100, value = 25),
                  sliderInput("home_game1",
                               label = "Home game for Player 2",
                              min = 0, max = 1, value = 0, step = 1)
                  ),
           column(2
           )
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  output$odds0 <- renderText({
    as.character(round(1/(predict(FINALmodel,
                      newdata = cbind(age_diff = ((input$age0 - input$age1) - std_params[1,1]) / std_params[2,1],
                                      height_diff = ((input$height0 - input$height1) - std_params[1,2]) / std_params[2,2],
                                      title_diff = ((input$titles0 - input$titles1) - std_params[1,3]) / std_params[2,3],
                                      diff_rank = ((input$rank0 - input$rank1) - std_params[1,4]) / std_params[2,4],
                                      diff_form_weighted = (((input$form_1_0 + input$form_5_0 + input$form_10_0) - (input$form_1_1 + input$form_5_1 + input$form_10_1)) - std_params[1,5]) / std_params[2,5],
                                      h2h = (((input$h2h0 + 1) / (input$h2h1 + 1)) - std_params[1,6]) / std_params[2,6],
                                      diff_conditions_wins = ((input$condition_wins0 - input$condition_wins1) - std_params[1,7]) / std_params[2,7],
                                      # fatigue_diff = (((input$fatigue_1_0 + input$fatigue_2_0/3) - (input$fatigue_1_1 + input$fatigue_2_1/3)) - std_params[1,8]) / std_params[2,8],
                                      home_game_diff = ((input$home_game0 - input$home_game1) - std_params[1,9]) / std_params[2,9]),
                      type = "prob")[1]*1.05), digits = 2))
  })
  
  output$odds1 <- renderText({
    as.character(round(1/(predict(FINALmodel,
                       newdata = cbind(age_diff = ((input$age0 - input$age1) - std_params[1,1]) / std_params[2,1],
                                       height_diff = ((input$height0 - input$height1) - std_params[1,2]) / std_params[2,2],
                                       title_diff = ((input$titles0 - input$titles1) - std_params[1,3]) / std_params[2,3],
                                       diff_rank = ((input$rank0 - input$rank1) - std_params[1,4]) / std_params[2,4],
                                       diff_form_weighted = (((input$form_1_0 + input$form_5_0 + input$form_10_0) - (input$form_1_1 + input$form_5_1 + input$form_10_1)) - std_params[1,5]) / std_params[2,5],
                                       h2h = (((input$h2h0 + 1) / (input$h2h1 + 1)) - std_params[1,6]) / std_params[2,6],
                                       diff_conditions_wins = ((input$condition_wins0 - input$condition_wins1) - std_params[1,7]) / std_params[2,7],
                                       # fatigue_diff = (((input$fatigue_1_0 + input$fatigue_2_0/3) - (input$fatigue_1_1 + input$fatigue_2_1/3)) - std_params[1,8]) / std_params[2,8],
                                       home_game_diff = ((input$home_game0 - input$home_game1) - std_params[1,9]) / std_params[2,9]),
                       type = "prob")[2]*1.05), digits = 2))
  })
  
  # output$winner <- renderText({
  #   paste("The winner is", round(exp(c(1, matrix(c(input$rank1, input$rank0))) %*% beta_logistic) / (1+exp(c(1,matrix(c(input$rank0, input$rank1))) %*% beta_logistic))))
  # })
  # 
  # output$eta <- renderText({
  #   paste("The eta is", exp(c(1, matrix(c(input$rank1, input$rank0))) %*% beta_logistic) / (1+exp(c(1,matrix(c(input$rank0, input$rank1))) %*% beta_logistic)))
  # })
}

shinyApp(ui = ui, server = server)