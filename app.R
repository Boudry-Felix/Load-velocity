library(shiny)
library(ggplot2)
library(plotly)

# Define UI for application that draws a Load-velocity plot
ui <- fluidPage(
  # Application title
  titlePanel("Load-velocity Model"),

  # Sidebar with input fields for speed and charge
  sidebarLayout(
    sidebarPanel(
      numericInput("speed", "Enter Speed (m/s):", value = 0, min = 0),
      numericInput("charge", "Enter Charge (kg):", value = 0, min = 0),
      actionButton("add", "Add to Plot"),
      actionButton("clear", "Clear Plot")
    ),

    # Main panel with tabs
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Summary",
          plotlyOutput("combinedPlot")
        ),
        tabPanel(
          "Plots",
          plotlyOutput("lvPlot"),
          plotlyOutput("fsPlot"),
          plotlyOutput("powerPlot")
        ),
        tabPanel(
          "Values",
          h4("Maximal Values"),
          verbatimTextOutput("maxForce"),
          verbatimTextOutput("maxSpeed"),
          verbatimTextOutput("maxPower")
        )
      )
    )
  )
)

# Define server logic required to draw the plot
server <- function(input, output) {
  # Data storage
  data <- reactiveVal(data.frame(speed = numeric(0), charge = numeric(0)))

  observeEvent(input$add, {
    new_data <- data()
    new_data <- rbind(new_data, data.frame(speed = input$speed, charge = input$charge))
    data(new_data)
  })

  observeEvent(input$clear, {
    data(data.frame(speed = numeric(0), charge = numeric(0)))
  })

  output$lvPlot <- renderPlotly({
    plot_data <- data()
    p <- ggplot(plot_data, aes(x = speed, y = charge)) +
      geom_point() +
      labs(x = "Speed (m/s)", y = "Charge (kg)", title = "Load-velocity Model") +
      theme(plot.title = element_text(hjust = 0.5)) # Center the plot title

    if (nrow(plot_data) > 1) {
      fit <- lm(charge ~ speed, data = plot_data)
      p <- p + geom_smooth(method = "lm", col = "blue", se = FALSE, formula = "y ~ x")

      ggplotly(p)
    }
  })

  output$fsPlot <- renderPlotly({
    plot_data <- data()
    plot_data$force <- plot_data$charge * 9.81 # Assuming charge is mass in kg and force is in Newtons
    p <- ggplot(plot_data, aes(x = speed, y = force)) +
      geom_point() +
      labs(x = "Speed (m/s)", y = "Force (N)", title = "Force-Speed Plot") +
      theme(plot.title = element_text(hjust = 0.5)) # Center the plot title

    if (nrow(plot_data) > 1) {
      fit <- lm(force ~ speed, data = plot_data)
      p <- p + geom_smooth(method = "lm", col = "blue", se = FALSE, formula = "y ~ x")

      ggplotly(p)
    }
  })

  output$powerPlot <- renderPlotly({
    plot_data <- data()
    plot_data$power <- plot_data$speed * plot_data$charge
    p <- ggplot(plot_data, aes(x = speed, y = power)) +
      geom_point() +
      labs(x = "Speed (m/s)", y = "Power (W)", title = "Power Evolution by Speed") +
      theme(plot.title = element_text(hjust = 0.5)) # Center the plot title

    if (nrow(plot_data) > 2) {
      fit <- lm(power ~ poly(speed, 2), data = plot_data)
      p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 2), col = "red", se = FALSE)

      ggplotly(p)
    }
  })

  output$combinedPlot <- renderPlotly({
    plot_data <- data()
    if (nrow(plot_data) > 0) {
      plot_data$force <- plot_data$charge * 9.81 # Assuming charge is mass in kg and force is in Newtons
      plot_data$power <- plot_data$speed * plot_data$charge

      # Combined plot
      p <- ggplot(plot_data) +
        geom_point(aes(x = speed, y = charge, color = "Charge")) +
        geom_point(aes(x = speed, y = force, color = "Force")) +
        geom_point(aes(x = speed, y = power, color = "Power")) +
        labs(x = "Speed (m/s)", y = "Value", title = "Combined Plot") +
        theme(plot.title = element_text(hjust = 0.5)) # Center the plot title

      if (nrow(plot_data) > 1) {
        fit1 <- lm(charge ~ speed, data = plot_data)
        fit2 <- lm(force ~ speed, data = plot_data)
        fit3 <- lm(power ~ poly(speed, 2), data = plot_data)

        p <- p + geom_smooth(aes(x = speed, y = charge, color = "Charge"), method = "lm", se = FALSE) +
          geom_smooth(aes(x = speed, y = force, color = "Force"), method = "lm", se = FALSE) +
          geom_smooth(aes(x = speed, y = power, color = "Power"), method = "lm", formula = y ~ poly(x, 2), se = FALSE)

        # Add regression equations to the legend
        eq1 <- paste0("Charge: y = ", round(coef(fit1)[1], 4), " + ", round(coef(fit1)[2], 4), "x")
        eq2 <- paste0("Force: y = ", round(coef(fit2)[1], 4), " + ", round(coef(fit2)[2], 4), "x")
        eq3 <- paste0("Power: y = ", round(coef(fit3)[1], 4), " + ", round(coef(fit3)[2], 4), "x + ", round(coef(fit3)[3], 4), "x^2")

        p <- p + scale_color_manual(
          name = "Legend",
          breaks = c("Charge", "Force", "Power"),
          values = c("Charge" = "blue", "Force" = "green", "Power" = "red"),
          labels = c(paste("Charge", eq1), paste("Force", eq2), paste("Power", eq3))
        )
      }

      ggplotly(p)
    } else {
      NULL
    }
  })

  output$maxForce <- renderText({
    plot_data <- data()
    if (nrow(plot_data) > 0) {
      plot_data$force <- plot_data$charge * 9.81 # Ensure force column exists
      fit <- lm(force ~ speed, data = plot_data)
      max_force <- max(fitted(fit))
      paste("Max Force: ", round(max_force, 4), " N")
    } else {
      "Max Force: N/A"
    }
  })

  output$maxSpeed <- renderText({
    plot_data <- data()
    if (nrow(plot_data) > 0) {
      plot_data$force <- plot_data$charge * 9.81 # Ensure force column exists
      fit <- lm(speed ~ force, data = plot_data)
      max_speed <- max(fitted(fit))
      paste("Max Speed: ", round(max_speed, 4), " m/s")
    } else {
      "Max Speed: N/A"
    }
  })

  output$maxPower <- renderText({
    plot_data <- data()
    if (nrow(plot_data) > 0) {
      plot_data$power <- plot_data$speed * plot_data$charge # Ensure power column exists
      fit <- lm(power ~ poly(speed, 2), data = plot_data)
      max_power <- max(fitted(fit))
      paste("Max Power: ", round(max_power, 4), " W")
    } else {
      "Max Power: N/A"
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
