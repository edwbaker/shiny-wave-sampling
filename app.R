library(shiny)
library(figuREd)
library(tuneR)

ui <- fluidPage(
    titlePanel("Sampled Wave"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("frequency",
                        "Frequency:",
                        min = 1,
                        max = 10,
                        value = 1),
            sliderInput("sampleRate",
                        "Sample Rate:",
                        min=1,
                        max=100,
                        value=10),
            sliderInput("adcLevels",
                        "ADC Levels:",
                        min=1,
                        max=256,
                        value=8)
        ),

        mainPanel(
           plotOutput("plot")
        )
    )
)

server <- function(input, output) {

    output$plot <- renderPlot({
        wave <- sine(input$frequency)
        plot_every <- reactive({floor(wave@samp.rate / input$sampleRate)})
        waveSampled(wave, plot_every(), input$adcLevels)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
