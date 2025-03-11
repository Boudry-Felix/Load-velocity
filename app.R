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
    
    # Show a plot of the generated model
    mainPanel(
      plotlyOutput("lvPlot"),
      plotlyOutput("fsPlot"),
      plotlyOutput("powerPlot"),
      plotlyOutput("combinedPlot")
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
      theme(plot.title = element_text(hjust = 0.5))  # Center the plot title
    
    if(nrow(plot_data) > 1) {
      fit <- lm(charge ~ speed, data = plot_data)
      p <- p + geom_smooth(method = "lm", col = "blue", se = FALSE, formula = 'y ~ x')
      
      # Add the regression equation to the plot
      eq <- paste0("y = ", round(coef(fit)[1], 4), " + ", round(coef(fit)[2], 4), "x")
      p <- p + annotate("text", x = max(plot_data$speed), y = max(plot_data$charge), label = eq, size = 4, color = "blue", hjust = 1, vjust = 1)
    }
    
    ggplotly(p)
  })
  
  output$fsPlot <- renderPlotly({
    plot_data <- data()
    if (nrow(plot_data) > 0) {
      plot_data$force <- plot_data$charge * 9.81  # Assuming charge is mass in kg and force is in Newtons
      p <- ggplot(plot_data, aes(x = speed, y = force)) +
        geom_point() +
        labs(x = "Speed (m/s)", y = "Force (N)", title = "Force-Speed Plot") +
        theme(plot.title = element_text(hjust = 0.5))  # Center the plot title
      
      if(nrow(plot_data) > 1) {
        fit <- lm(force ~ speed, data = plot_data)
        p <- p + geom_smooth(method = "lm", col = "blue", se = FALSE, formula = 'y ~ x')
        
        # Add the regression equation to the plot
        eq <- paste0("y = ", round(coef(fit)[1], 4), " + ", round(coef(fit)[2], 4), "x")
        p <- p + annotate("text", x = max(plot_data$speed), y = max(plot_data$force), label = eq, size = 4, color = "blue", hjust = 1, vjust = 1)
      }
      
      ggplotly(p)
    } else {
      NULL
    }
  })
  
  output$powerPlot <- renderPlotly({
    plot_data <- data()
    if (nrow(plot_data) > 0) {
      plot_data$power <- plot_data$speed * plot_data$charge
      p <- ggplot(plot_data, aes(x = speed, y = power)) +
        geom_point() +
        labs(x = "Speed (m/s)", y = "Power (W)", title = "Power Evolution by Speed") +
        theme(plot.title = element_text(hjust = 0.5))  # Center the plot title
      
      if(nrow(plot_data) > 2) {
        fit <- lm(power ~ poly(speed, 2), data = plot_data)
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 2), col = "red", se = FALSE)
        
        # Add the regression equation to the plot
        eq <- paste0("y = ", round(coef(fit)[1], 4), " + ", round(coef(fit)[2], 4), "x + ", round(coef(fit)[3], 4), "x^2")
        p <- p + annotate("text", x = max(plot_data$speed), y = max(plot_data$power), label = eq, size = 4, color = "red", hjust = 1, vjust = 1)
      }
      
      ggplotly(p)
    } else {
      NULL
    }
  })

  output$combinedPlot <- renderPlotly({
    plot_data <- data()
    if (nrow(plot_data) > 0) {
      plot_data$force <- plot_data$charge * 9.81  # Assuming charge is mass in kg and force is in Newtons
      plot_data$power <- plot_data$speed * plot_data$charge
      
      p1 <- ggplot(plot_data, aes(x = speed, y = charge)) +
        geom_point() +
        labs(x = "Speed (m/s)", y = "Charge (kg)", title = "Load-velocity Model") +
        theme(plot.title = element_text(hjust = 0.5))  # Center the plot title
      
      if(nrow(plot_data) > 1) {
        fit1 <- lm(charge ~ speed, data = plot_data)
        p1 <- p1 + geom_smooth(method = "lm", col = "blue", se = FALSE, formula = 'y ~ x')
        eq1 <- paste0("y = ", round(coef(fit1)[1], 4), " + ", round(coef(fit1)[2], 4), "x")
        p1 <- p1 + annotate("text", x = max(plot_data$speed), y = max(plot_data$charge), label = eq1, size = 4, color = "blue", hjust = 1, vjust = 1)
      }
      
      p2 <- ggplot(plot_data, aes(x = speed, y = force)) +
        geom_point() +
        labs(x = "Speed (m/s)", y = "Force (N)", title = "Force-Speed Plot") +
        theme(plot.title = element_text(hjust = 0.5))  # Center the plot title
      
      if(nrow(plot_data) > 1) {
        fit2 <- lm(force ~ speed, data = plot_data)
        p2 <- p2 + geom_smooth(method = "lm", col = "blue", se = FALSE, formula = 'y ~ x')
        eq2 <- paste0("y = ", round(coef(fit2)[1], 4), " + ", round(coef(fit2)[2], 4), "x")
        p2 <- p2 + annotate("text", x = max(plot_data$speed), y = max(plot_data$force), label = eq2, size = 4, color = "blue", hjust = 1, vjust = 1)
      }
      
      p3 <- ggplot(plot_data, aes(x = speed, y = power)) +
        geom_point() +
        labs(x = "Speed (m/s)", y = "Power (W)", title = "Power Evolution by Speed") +
        theme(plot.title = element_text(hjust = 0.5))  # Center the plot title
      
      if(nrow(plot_data) > 2) {
        fit3 <- lm(power ~ poly(speed, 2), data = plot_data)
        p3 <- p3 + geom_smooth(method = "lm", formula = y ~ poly(x, 2), col = "red", se = FALSE)
        eq3 <- paste0("y = ", round(coef(fit3)[1], 4), " + ", round(coef(fit3)[2], 4), "x + ", round(coef(fit3)[3], 4), "x^2")
        p3 <- p3 + annotate("text", x = max(plot_data$speed), y = max(plot_data$power), label = eq3, size = 4, color = "red", hjust = 1, vjust = 1)
      }
      
      combined_plot <- subplot(ggplotly(p1), ggplotly(p2), ggplotly(p3), nrows = 3, shareX = TRUE)
      combined_plot
    } else {
      NULL
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
