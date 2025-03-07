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
      plotlyOutput("lvPlot")
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
    p <- ggplot(plot_data, aes(x = charge, y = speed)) +
      geom_point() +
      labs(x = "Charge (kg)", y = "Speed (m/s)", title = "Load-velocity Model") +
      theme_minimal()
    
    if(nrow(plot_data) > 1) {
      fit <- lm(speed ~ charge, data = plot_data)
      p <- p + geom_smooth(method = "lm", col = "blue", se = FALSE, formula = 'y ~ x')
    }
    
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)