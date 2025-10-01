library(shiny)
library(ggplot2)

ui <- fluidPage(

    titlePanel("Simulación Pobreza y Distribución del Ingreso"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("mean",
                        "Ingreso promedio:",
                        min = -5,
                        max = 5,
                        value = 0,
                        step = 0.1),
            sliderInput("sd",
                        "Desvío estándar del ingreso:",
                        min = 0.5,
                        max = 3,
                        value = 1,
                        step = 0.1)
        ),

        mainPanel(
           plotOutput("distPlot"),
           textOutput("povertyInfo")
        )
    )
)

server <- function(input, output) {

    output$distPlot <- renderPlot({
        x_vals <- seq(-10, 10, length.out = 400)
        density_data <- data.frame(
            x = x_vals,
            y = dnorm(x_vals, mean = input$mean, sd = input$sd)
        )

        linea_de_pobreza <- -2

        p <- ggplot(density_data, aes(x = x, y = y)) +
            geom_line() +
            geom_vline(xintercept = linea_de_pobreza, color = "red", linetype = "dashed") +
            geom_ribbon(data = subset(density_data, x < linea_de_pobreza),
                        aes(ymax = y), ymin = 0,
                        fill = "red", alpha = 0.5) +
            coord_cartesian(xlim = c(-10, 10), ylim = c(0, 0.85)) + # Fix axes
            labs(title = "Distribución del ingreso",
                 x = "Ingreso",
                 y = "Densidad") +
            theme_minimal()
        
        print(p)
    })
    
    output$povertyInfo <- renderText({
        linea_de_pobreza <- -2
        percentage <- pnorm(linea_de_pobreza, mean = input$mean, sd = input$sd)
        paste0("Porcentaje de la pobleción debajo de la línea de pobreza: ", round(percentage * 100, 2), "%")
    })
}

shinyApp(ui = ui, server = server)