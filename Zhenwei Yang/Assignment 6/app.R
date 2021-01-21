library(shiny)
source("functions.R")

ui <- fluidPage(
  titlePanel(h1("Simulated Linear Regression")),
  
  sidebarLayout(
    sidebarPanel(
      helpText("In this app, you are allowed to simulate a dataset yourself and do the linear regression plot. 
               The parameters that you can vary are the number of observations, regression coefficients, 
               standard deviation (sd) of the error term and the mean and sd of independent varaible.
               Note that independent variable is subject to a normal distribution."),
      numericInput("num", h4("number of observations:"), value = 1000),
      numericInput("b0", h4("regression coefficient for the intercept"), value = 1),
      numericInput("b1", h4("regression coefficient for x"), value = 1),
      numericInput("meanx", h4("mean of xs"), value = 1),
      numericInput("sdx", h4("standard deviation for x"), value = 1),
      numericInput("sde", h4("standard deviation of the error term"), value = 1)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {

  dataInput <- reactive({
    generate_data(input$num, input$b0, input$b1, input$meanx, input$sdx, input$sde)
  })
  
  output$plot <- renderPlot({
    plot(y ~ x,  data = dataInput()[[1]])
    abline(b = dataInput()[[3]], a = dataInput()[[2]], col = "red")
  })
  
}

shinyApp(ui, server)
