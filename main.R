library(shiny)

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
      plotOutput("lvPlot")
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
  
  output$lvPlot <- renderPlot({
    plot_data <- data()
    plot(plot_data$charge, plot_data$speed, 
         xlab = "Charge (kg)", ylab = "Speed (m/s)", 
         main = "Load-velocity Model", pch = 19)
    if(nrow(plot_data) > 1) {
      # Add regression line
      fit <- lm(speed ~ charge, data = plot_data)
      abline(fit, col = "blue")
      
      # Optionally, add the regression equation to the plot
      eq <- paste0("y = ", round(coef(fit)[1], 2), " + ", round(coef(fit)[2], 2), "x")
      legend("topleft", legend = eq, bty = "n")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
