library(shiny)
library(tidyverse)
library(distributions3)

server <- function(input,output,session){

  RVs <- reactiveValues(limits = NULL)

  observeEvent(input$distribution, {
    ## Get needed arguments from distribution
    RVs$X_args <- as.list(args(input$distribution))
    ## Remove any NULL arguments
    RVs$X_args <- RVs$X_args[!map_lgl(RVs$X_args, is.null)]
  })

  ## Create UI elements that let users specify parameters
  output$distribution_parameters <-
    renderUI({
      if(!is.null(RVs$X_args)){
        lapply(1:length(RVs$X_args),
               function(i)
                 numericInput(inputId = names(RVs$X_args)[i],
                              label = names(RVs$X_args)[i],
                              value = ifelse(is.numeric(RVs$X_args[[i]]),
                                             RVs$X_args[[i]],
                                             1))
        )
      }
    })

  output$choose_p <- renderUI({
    numericInput(inputId = "q",
                 label = "Choose percentile for cut-off",
                 value = if_else(input$distribution %in% c('FisherF', 'StudentsT'), 0.1, 0.01),
                 min = if_else(input$distribution %in% c('FisherF', 'StudentsT'), 0.05, 0.001), max = 0.5)
  })

  observeEvent({
    # RVs$X_args
    # input$distribution
    input$GO
  }, {
    RVs$X <- do.call(eval(parse(text = paste0("function(...) ", input$distribution, "(...)"))),
                     args = setNames(map(names(RVs$X_args), function(x) as.numeric(input[[x]])),
                                     names(RVs$X_args)))
  })

  output$plot_range <- renderUI({
    if(is_distribution(RVs$X)){
      sliderInput(inputId = "plot_range",
                  label = "Choose x-axis range",
                  min = case_when(support(RVs$X)[1] == -Inf ~ round(quantile(RVs$X, p = 0.01)*2, digits = 3),
                                  TRUE ~support(RVs$X)[1]),
                  max = case_when(support(RVs$X)[2] == Inf ~ round(quantile(RVs$X, p = 1-0.01)*2, digits = 3),
                                  TRUE ~ support(RVs$X)[2]),
                  value = c(case_when(#!is.null(RVs$limits) ~ RVs$limits[1],
                                      support(RVs$X)[1] == -Inf ~ round(quantile(RVs$X, p = 0.01),digits = 3),
                                      TRUE ~ support(RVs$X)[1]),
                            case_when(#!is.null(RVs$limits) ~ RVs$limits[2],
                                      support(RVs$X)[2] == Inf ~ round(quantile(RVs$X, p = 1-0.01), digits = 3),
                                      TRUE ~ support(RVs$X)[2])))
    }
  })

  observeEvent(input$update_limits,{
    RVs$limits <- input$plot_range
  })

  ## Plot PDF of distribution chosen
  output$PDF <- renderPlot({
    if(is_distribution(RVs$X))
      plot_pdf(RVs$X, limits = RVs$limits, p = input$q)

  })

  output$X <- renderText({paste(RVs$X, collapse = ", ")})
}

ui <- fluidPage(
  titlePanel("PDF/PMF of A Few Distributions"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'distribution',
                  label = 'Choose a distribution',
                  choices = c('Beta', 'Binomial', 'ChiSquare', 'Exponential', 'FisherF',
                              'Gamma', 'Logistic', 'LogNormal', 'NegativeBinomial',
                              'Normal', 'Poisson', 'StudentsT', 'Weibull'),
                  selected = 'Binomial'),
      uiOutput("distribution_parameters"),
      actionButton("GO", "GO"),
      checkboxInput("extra", label = "Extra Options", value = FALSE),
      conditionalPanel("input.extra", uiOutput("choose_p")),
      conditionalPanel("input.extra", uiOutput("plot_range")),
      conditionalPanel("input.extra", actionButton("update_limits", label = "Update x-axis"))
    ),
    mainPanel(
      plotOutput("PDF")
    )
  )
)

shinyApp(ui = ui, server = server)
