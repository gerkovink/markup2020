library(rsconnect)
library(shiny)
library(tidyverse)
library(psych)

dat <- readRDS("BankChurners_adapted.RDS")
vars <- list(names(dat))
vars <- vars[[1]][1:20]

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("EDA credit card data set"),

    # Drop down menu with variables 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "variable",
                        label = "Select variable:",
                        choices = vars),
            h4("Introduction"),
            p("This Shiny app is designed to do an intial exploration of the Bankchurners data set. Select a variable to see its description, distribution and descriptive statistics. " )
            
            
            
        ),

        # Show a plot of the distribution of the variable
        mainPanel(
            h5("Variable description", align = "center"),
            verbatimTextOutput("intro"),
           h5("Variable distribution", align = "center"),
           plotOutput("hist"),
           h5("descriptive statistics", align = "center"),
           verbatimTextOutput("desc")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$hist <- renderPlot({
        
        x    <- dat[, input$variable]

        if (is.numeric(x) == TRUE) {
        hist(x, main = "")
        } else {
        barplot(table(x))
    }
    })
    output$desc <- renderPrint({summary(dat[,input$variable])})
    output$intro <- renderPrint({comment(dat[,input$variable])
})
}


deployApp()
