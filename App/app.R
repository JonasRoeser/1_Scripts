# Because of OneDrive we need to load from two different paths
load("beta_logistic.RData")

# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = "stylesheet.css",
  titlePanel(h1("The Machine", align = "center")),
  
  fluidRow(
    column(12, align = "center",
           h2("Offered Bedding Odds", align = "center"),
           textOutput("winner"),
           p("- : -"),
           textOutput("eta")),
    column(12, align = "center",
           column(2
           ),
           column(4,
                  h2("Player 1 Inputs", align = "center"),
                  numericInput("name0",
                               label = "Name of Player 1",
                               value = 1),
                  numericInput("rank0",
                               label = "Rank of Player 1",
                               value = 1),
                  numericInput("form0",
                               label = "Form of Player 1",
                               value = 1),
                  numericInput("condition_wins0",
                               label = "Form on these Conditions of Player 1",
                               value = 1),
                  numericInput("h2h0",
                               label = "Head to Head for Player 1",
                               value = 1)
                  ),
           column(4,
                  h2("Player 2 Inputs", align = "center"),
                  numericInput("name1",
                               label = "Name of Player 2",
                               value = 1),
                  numericInput("rank1",
                               label = "Rank of Player 2",
                               value = 1),
                  numericInput("form1",
                               label = "Form of Player 2",
                               value = 1),
                  numericInput("condition_wins1",
                               label = "Form on these Conditions of Player 2",
                               value = 1),
                  numericInput("h2h1",
                               label = "Head to Head for Player 2",
                               value = 1)
                  ),
           column(2
           )
    )
  )
)

server <- function(input, output) {
  
  output$winner <- renderText({
    paste("The winner is", round(exp(c(1, matrix(c(input$rank1, input$rank0))) %*% beta_logistic) / (1+exp(c(1,matrix(c(input$rank0, input$rank1))) %*% beta_logistic))))
  })
  
  output$eta <- renderText({
    paste("The eta is", exp(c(1, matrix(c(input$rank1, input$rank0))) %*% beta_logistic) / (1+exp(c(1,matrix(c(input$rank0, input$rank1))) %*% beta_logistic)))
  })
  
}

shinyApp(ui = ui, server = server)