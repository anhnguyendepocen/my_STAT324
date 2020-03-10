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
            sliderInput("p", "&pi;", value = 0.1,
                        min = 0, max = 1, step = 0.05),
            sliderInput("n", "n", value = 5,
                        min = 1, max = 20, step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        tmp <- ggplot(data = tibble(x = 0:input$n,
                                    y = dbinom(x = 0:input$n, size = input$n, prob = input$p)),
                      aes(x = x, y = y)) +
            geom_bar(stat = "identity") +
            scale_x_continuous(limits = c(-0.5, 20.5)) +
            theme_bw()

        # tmp_build <- ggplot_build(tmp)
        #
        # x_cutoffs <- tmp_build$data[[1]] %>% select(xmin, xmax) %>% unlist() %>% round(digits = 6) %>% unique()
        #
        # if(input$show_p == "Yes"){
        #     tmp <- tmp + geom_vline(aes(xintercept = quantile(x, input$p)))
        # }
        #
        # tmp1 <- ggplot(data = sim_data$dat,
        #                aes(x = x, y = "1")) +
        #     scale_x_continuous(limits = c(-50, 50)) +
        #     geom_vline(xintercept = x_cutoffs, color = "red", size = 0.1) +
        #     geom_point(position = sim_data$pos_jit) +
        #     labs(y = "") +
        #     theme(panel.grid.major.y = element_blank(),
        #           axis.ticks.y = element_blank(),
        #           axis.text.y = element_blank())

        #return(tmp + tmp1 + plot_layout(ncol = 1, heights = c(2, 1)))

        return(tmp)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
