library(shiny)
library(tidyverse)
library(distributions3)

server <- function(input,output,session){

  X <- Normal(170, sqrt(225))

  base_plot <- plot_pdf(X)

  ## Plot PDF of distribution chosen
  output$PDF <- renderPlot({
     base_plot +
      geom_area(aes(y = if_else(x >= input$range[1] & x <= input$range[2], y, 0)),
                alpha = 0.3) +
      geom_vline(xintercept = input$x, color = "blue") +
      geom_vline(xintercept = input$range, color = "red", linetype = "dashed")
  })
}

ui <- fluidPage(
  fluidRow(
    column(10, align = "center", offset = 1,
           plotOutput("PDF"),
           sliderInput(inputId = "range",
                       label = "P(lower < X < upper)",
                       min = 125, max = 212,
                       value = c(150,200), width = "82%"),
           numericInput(inputId = "x",
                        label = "P(X = x)",
                        min = 125, max = 212, value = 182)
    )
  )
)

shinyApp(ui = ui, server = server)
