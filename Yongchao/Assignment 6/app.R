library(shiny)

# Define UI for app ----
ui <- fluidPage(
  
  titlePanel("Plots in Base R"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Example plots of a built-in dataset"),
      
      selectInput("plot",
                  label = "Choose a plot to display",
                  choices = list("Scatterplot", 
                                 "Histogram",
                                 "Boxplot", 
                                 "Barplot"),
                  selected = "Scatterplot")
      
    ),
    
    mainPanel(
      
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    if (input$plot == "Scatterplot") {
      plot(mtcars$wt, mtcars$mpg, pch = 16,
           xlab = "Weight (1000 lbs)",
           ylab = "Miles/(US) gallon")
    } else if (input$plot == "Histogram") {
      hist(mtcars$mpg, breaks = 10,
           xlab = "Miles/(US) gallon",
           main = "")
    } else if (input$plot == "Boxplot") {
      boxplot(mtcars$mpg ~ mtcars$cyl,
              xlab = "Number of cylinders",
              ylab = "Miles/(US) gallon")
    } else {
      barplot(table(mtcars$cyl),
              xlab = "Number of cylinders")
    }
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)