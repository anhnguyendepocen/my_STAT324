library(shiny)
library(tidyverse)
library(distributions3)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("t-distribution"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("df",
                        "Degrees of Freedom",
                        min = 1,
                        max = 150,
                        step = 1,
                        value = 2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    base_plot <- ggplot() +
        geom_pdf(d = Normal(), aes(color = "N"), limits = c(-5,5))

    output$distPlot <- renderPlot({
        dfs <- input$df

        Tdist <- StudentsT(df = dfs)

        base_plot +
            geom_pdf(d = Tdist, aes(color = "t"), limits = c(-5,5)) +
            scale_color_manual("",
                               values = c("N" = "black", "t" = "red"),
                               labels = c("N(0,1)", bquote("t"[.(dfs)]))) +
            theme_bw()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
