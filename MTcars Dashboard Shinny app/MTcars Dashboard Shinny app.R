library(shiny)
library(dplyr)
library(ggplot2)

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("MTCars Dashboard"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel with sliders
    sidebarPanel(
      sliderInput("cyl_input", "Select Number of Cylinders:",
                  min = min(mtcars$cyl), max = max(mtcars$cyl),
                  value = c(min(mtcars$cyl), max(mtcars$cyl)),
                  step = 1),
      
      sliderInput("mpg_input", "Select MPG Range:",
                  min = min(mtcars$mpg), max = max(mtcars$mpg),
                  value = c(min(mtcars$mpg), max(mtcars$mpg)),
                  step = 1)
    ),
    
    # Main panel with plots as cards
    mainPanel(
      fluidRow(
        column(width = 6, plotOutput("plot1", height = 300)),
        column(width = 6, plotOutput("plot2", height = 300))
      ),
      fluidRow(
        column(width = 6, plotOutput("plot3", height = 300)),
        column(width = 6, plotOutput("plot4", height = 300))
      ),
      fluidRow(
        column(width = 6, plotOutput("plot5", height = 300)),
        column(width = 6, plotOutput("plot6", height = 300))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive function to filter data based on input sliders
  filtered_data <- reactive({
    mtcars %>%
      filter(cyl >= input$cyl_input[1] & cyl <= input$cyl_input[2] &
               mpg >= input$mpg_input[1] & mpg <= input$mpg_input[2])
  })
  
  # Plot 1: Scatterplot of mpg vs. hp
  output$plot1 <- renderPlot({
    ggplot(filtered_data(), aes(x = hp, y = mpg)) +
      geom_point() +
      labs(title = "Scatterplot of MPG vs. Horsepower")
  })
  
  # Plot 2: Histogram of mpg
  output$plot2 <- renderPlot({
    ggplot(filtered_data(), aes(x = mpg)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
      labs(title = "Histogram of MPG")
  })
  
  # Plot 3: Bar chart of number of cylinders
  output$plot3 <- renderPlot({
    ggplot(filtered_data(), aes(x = factor(cyl))) +
      geom_bar(fill = "salmon", color = "black") +
      labs(title = "Bar Chart of Number of Cylinders")
  })
  
  # Plot 4: Boxplot of mpg by number of gears
  output$plot4 <- renderPlot({
    ggplot(filtered_data(), aes(x = factor(gear), y = mpg)) +
      geom_boxplot(fill = "lightgreen", color = "black") +
      labs(title = "Boxplot of MPG by Number of Gears")
  })
  
  # Plot 5: Line plot of mpg over time (row number)
  output$plot5 <- renderPlot({
    ggplot(filtered_data(), aes(x = seq_along(mpg), y = mpg)) +
      geom_line(color = "darkorange") +
      labs(title = "Line Plot of MPG Over Time")
  })
  
  # Plot 6: Scatterplot of mpg vs. weight
  output$plot6 <- renderPlot({
    ggplot(filtered_data(), aes(x = wt, y = mpg)) +
      geom_point() +
      labs(title = "Scatterplot of MPG vs. Weight")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
