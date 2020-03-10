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
library(patchwork)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Histogram"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons("n_or_width", "Control number of bins or binwidth?",
                         choices = c("bins", "binwidth")),
            conditionalPanel("input.n_or_width == 'bins'",
                             sliderInput("bins",
                                         "Number of bins:",
                                         min = 1,
                                         max = 100,
                                         value = 50)),
            conditionalPanel("input.n_or_width == 'binwidth'",
                             sliderInput("binwidth",
                                         "Binwidth",
                                         min = 0.1,
                                         max = 50,
                                         value = 1)),
            sliderInput("mean", "Mean", value = 0,
                        min = -10, max = 10, step = 0.1),
            sliderInput("variance", "Variance", value = 50,
                        min = 0.1, max = 100, step = 0.1),
            radioButtons("show_p", "Show percentile?", choices = c("No", "Yes")),
            conditionalPanel("input.show_p == 'Yes'",
                             sliderInput("p", "Show percentile", value = 0.5,
                                         min = 0.01, max = 1, step = 0.01))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    sim_data <- reactiveValues()

    observeEvent({
        input$mean
        input$variance
    }, {
        sim_data$dat <- tibble(x = rnorm(300, mean = input$mean, sd = sqrt(input$variance)))
        sim_data$pos_jit <- position_jitter(height = 0.4)
    })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        tmp <- ggplot(data = sim_data$dat,
                      aes(x = x)) +
            coord_cartesian(xlim = c(-50, 50))

        if(input$n_or_width == "bins"){
            tmp <- tmp + geom_histogram(color = "black", boundary = 0, bins = input$bins, na.rm = TRUE)
        } else {
            tmp <- tmp + geom_histogram(color = "black", boundary = 0, binwidth = input$binwidth, na.rm = TRUE)
        }

        tmp_build <- ggplot_build(tmp)

        x_cutoffs <- tmp_build$data[[1]] %>% select(xmin, xmax) %>% unlist() %>% round(digits = 6) %>% unique()

        if(input$show_p == "Yes"){
            tmp <- tmp + geom_vline(aes(xintercept = quantile(x, input$p)))
        }

        tmp1 <- ggplot(data = sim_data$dat,
                       aes(x = x, y = "1")) +
            scale_x_continuous(limits = c(-50, 50)) +
            geom_vline(xintercept = x_cutoffs, color = "red", size = 0.1) +
            geom_point(position = sim_data$pos_jit) +
            labs(y = "") +
            theme(panel.grid.major.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank())

        return(tmp + tmp1 + plot_layout(ncol = 1, heights = c(2, 1)))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
