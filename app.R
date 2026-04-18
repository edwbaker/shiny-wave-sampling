library(shiny)
library(figuREd)
library(tuneR)
library(shinynhm)

ui <- nhm_page(
    title = "Wave Sampling Demonstration",
    description = "This app demonstrates the effects of sampling a sine wave at different frequencies, sample rates, and ADC levels.",
    subbrand = "NHM Living Labs",
    footer = FALSE,
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
           nhm_panel(
               plotOutput("plot")
           )
        )
    )
)

server <- function(input, output) {
    output$plot <- renderPlot({
        nhm_par()
        wave <- sine(input$frequency)
        plot_every <- floor(wave@samp.rate / input$sampleRate)
        waveSampled(wave, plot_every, input$adcLevels, wave_col = nhm_colours()$lime)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
