# Because of OneDrive we need to load from two different paths
load("beta_logistic.RData")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel(h1("Betting-Odds", align = "center")),
  
  fluidRow(
    column(3,
           h2("Offered Bedding Odds", align = "center"),
           textOutput("winner"),
           textOutput("eta")),
    column(3,
           h2("Player 0 Inputs", align = "center"),
              numericInput("rank0",
                           label = "Rank of Player 0",
                           value = 1),
              h2("Player 1 Inputs", align = "center"),
              numericInput("rank1",
                           label = "Rank of Player 1",
                           value = 1))
  )
)

server <- function(input, output) {
  
  output$winner <- renderText({
    paste("The winner is", round(exp(c(1, matrix(c(input$rank0, input$rank1))) %*% beta_logistic) / (1+exp(c(1,matrix(c(input$rank0, input$rank1))) %*% beta_logistic))))
  })
  
  output$eta <- renderText({
    paste("The eta is", exp(c(1, matrix(c(input$rank0, input$rank1))) %*% beta_logistic) / (1+exp(c(1,matrix(c(input$rank0, input$rank1))) %*% beta_logistic)))
  })
  
}

shinyApp(ui = ui, server = server)