library(shiny)
library(figuREd)
library(tuneR)
library(shinynhm)

ui <- nhm_page(
    title = "Wave Sampling and Aliasing Demonstration",
    description = "This app demonstrates the effects of sampling a sine wave at different frequencies, sample rates, and ADC levels. It also explores aliasing and the role of anti-aliasing filters.",
    subbrand = "NHM Living Labs",
    footer = FALSE,
    tabsetPanel(
        id = "mainTabs",
        type = "pills",
        tabPanel("Sampling",
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
        ),
        tabPanel("Aliasing",
            sidebarLayout(
                sidebarPanel(
                    sliderInput("aliasFreq",
                                "Signal Frequency (Hz):",
                                min = 1,
                                max = 50,
                                value = 5),
                    sliderInput("aliasSampleRate",
                                "Sample Rate (Hz):",
                                min = 2,
                                max = 100,
                                value = 40),
                    selectInput("filterType",
                                "Anti-aliasing filter:",
                                choices = c("None" = "none",
                                            "Ideal (brick-wall)" = "ideal",
                                            "Butterworth" = "butterworth",
                                            "Chebyshev Type I" = "chebyshev1",
                                            "Bessel" = "bessel"),
                                selected = "none"),
                    conditionalPanel(
                        condition = "input.filterType != 'none' && input.filterType != 'ideal'",
                        sliderInput("filterOrder",
                                    "Filter Order:",
                                    min = 1,
                                    max = 10,
                                    value = 4)
                    ),
                    helpText("Aliasing occurs when the signal frequency exceeds half the sample rate (the Nyquist frequency). An anti-aliasing low-pass filter removes frequencies above the Nyquist limit before sampling.")
                ),
                mainPanel(
                    nhm_panel(
                        plotOutput("aliasPlot", height = "200px")
                    ),
                    nhm_panel(
                        plotOutput("aliasReconstructed", height = "200px")
                    ),
                    nhm_panel(
                        plotOutput("aliasFiltered", height = "200px")
                    )
                )
            )
        )
    )
)

server <- function(input, output) {

    # Filter magnitude response at a given frequency
    filterGain <- function(f, nyquist, type, order = 4) {
        if (type == "none" || nyquist == 0) return(1)
        if (type == "ideal") return(if (f <= nyquist) 1 else 0)
        ratio <- f / nyquist
        if (type == "butterworth") {
            return(1 / sqrt(1 + ratio^(2 * order)))
        }
        if (type == "chebyshev1") {
            eps <- 0.5  # 0.5 dB ripple approx
            # Chebyshev polynomial via cosh/cos
            if (ratio <= 1) {
                Tn <- cos(order * acos(ratio))
            } else {
                Tn <- cosh(order * acosh(ratio))
            }
            return(1 / sqrt(1 + eps^2 * Tn^2))
        }
        if (type == "bessel") {
            # Approximate Bessel as Butterworth with gentler slope
            return(1 / sqrt(1 + (ratio)^(2 * max(order - 1, 1))))
        }
        return(1)
    }

    output$plot <- renderPlot({
        nhm_par()
        wave <- sine(input$frequency)
        plot_every <- floor(wave@samp.rate / input$sampleRate)
        waveSampled(wave, plot_every, input$adcLevels, wave_col = nhm_colours()$lime)
    })

    # Aliasing tab
    output$aliasPlot <- renderPlot({
        nhm_par()
        par(mar = c(7, 4, 3, 1))
        cols <- nhm_colours()
        freq <- input$aliasFreq
        sr <- input$aliasSampleRate
        nyquist <- sr / 2
        ftype <- input$filterType
        forder <- if (is.null(input$filterOrder)) 4 else input$filterOrder
        gain <- filterGain(freq, nyquist, ftype, forder)

        # Continuous signal
        t_cont <- seq(0, 1, length.out = 1000)
        y_cont <- sin(2 * pi * freq * t_cont)

        # Sample points
        t_samp <- seq(0, 1, by = 1 / sr)
        y_samp <- gain * sin(2 * pi * freq * t_samp)

        plot(t_cont, y_cont, type = "l", col = cols$lime,
             xlab = "Time (s)", ylab = "Amplitude", ylim = c(-1.2, 1.2),
             main = paste0("Original signal (", freq, " Hz) with sample points"))
        points(t_samp, y_samp, pch = 19, col = cols$cyan, cex = 1.5)
        abline(h = 0, col = cols$muted)
        if (freq > nyquist) {
            mtext(paste0("Nyquist = ", nyquist, " Hz \u2014 ALIASING"),
                  side = 1, line = 5.5, col = cols$danger, cex = 1.2)
        } else {
            mtext(paste0("Nyquist = ", nyquist, " Hz \u2014 OK"),
                  side = 1, line = 5.5, col = cols$lime, cex = 1.2)
        }
    })

    output$aliasReconstructed <- renderPlot({
        nhm_par()
        par(mar = c(7, 4, 3, 1))
        cols <- nhm_colours()
        freq <- input$aliasFreq
        sr <- input$aliasSampleRate
        nyquist <- sr / 2
        ftype <- input$filterType
        forder <- if (is.null(input$filterOrder)) 4 else input$filterOrder
        gain <- filterGain(freq, nyquist, ftype, forder)

        # Sample points
        t_samp <- seq(0, 1, by = 1 / sr)
        y_samp <- gain * sin(2 * pi * freq * t_samp)

        # Reconstructed (aliased) frequency — phase flips on each fold
        alias_freq <- abs(freq - sr * round(freq / sr))
        sign_flip <- (-1)^round(freq / sr)

        t_cont <- seq(0, 1, length.out = 1000)
        y_recon <- gain * sign_flip * sin(2 * pi * alias_freq * t_cont)

        # True signal for comparison
        y_true <- sin(2 * pi * freq * t_cont)

        plot(t_cont, y_true, type = "l", col = cols$muted, lty = 2,
             xlab = "Time (s)", ylab = "Amplitude", ylim = c(-1.2, 1.2),
             main = paste0("Reconstructed signal",
                           if (freq > nyquist && gain > 0.01)
                               paste0(" (alias = ", alias_freq, " Hz)") else ""))
        lines(t_cont, y_recon, col = cols$pink, lwd = 2)
        points(t_samp, y_samp, pch = 19, col = cols$cyan, cex = 1.2)
        abline(h = 0, col = cols$muted)
        mtext("Original", side = 1, line = 5.5, adj = 0.4, col = cols$muted, cex = 1)
        mtext("Reconstructed", side = 1, line = 5.5, adj = 0.6, col = cols$pink, cex = 1)
    })

    output$aliasFiltered <- renderPlot({
        nhm_par()
        cols <- nhm_colours()
        freq <- input$aliasFreq
        sr <- input$aliasSampleRate
        nyquist <- sr / 2
        ftype <- input$filterType
        forder <- if (is.null(input$filterOrder)) 4 else input$filterOrder

        # Frequency axis
        freqs <- 0:50
        spectrum <- ifelse(freqs == freq, 1, 0)

        # Apply filter gain to spectrum
        gains <- sapply(freqs, function(f) filterGain(f, nyquist, ftype, forder))
        filtered_spectrum <- spectrum * gains

        bar_cols <- ifelse(freqs > nyquist, cols$pink, cols$cyan)

        barplot(filtered_spectrum, names.arg = freqs, col = bar_cols, border = bar_cols,
                xlab = "Frequency (Hz)", ylab = "Magnitude",
                main = paste0("Frequency spectrum",
                              if (ftype != "none") paste0(" (", ftype, " filter)") else ""),
                ylim = c(0, 1.1))
        # Show filter response curve
        if (ftype != "none") {
            f_fine <- seq(0, 50, length.out = 500)
            g_fine <- sapply(f_fine, function(f) filterGain(f, nyquist, ftype, forder))
            # barplot x-coords: each bar width=1, spacing handled by barplot
            lines(f_fine * 1.2 + 0.5, g_fine, col = cols$lime, lwd = 2, lty = 2)
        }
        abline(v = (nyquist + 0.5) * 1.2, col = cols$lime, lwd = 2, lty = 2)
        text(nyquist * 1.2, 1.05, paste0("Nyquist\n", nyquist, " Hz"),
             col = cols$lime, pos = 4, cex = 0.9)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
