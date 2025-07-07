# Load libraries
library(tidyverse)
library(lubridate)
library(viridis) #colour pallette
library(shiny)
library(bslib)
library(plotly)
library(readr)
library(shinyjs)
library(shinythemes)
library(readr)


# Reading and loading from URL to the CSV data library(readr)
url = "https://data.sfgov.org/api/views/88g8-5mnd/rows.csv?accessType=DOWNLOAD"
data = read_csv(url)

data = Employee_Compensation

# Data Validation: Filter data for years 2017 to 2019
data_valid <- data %>% filter(Year %in% c(2017, 2018, 2019))

# Rename columns to replace periods with underscores
data_standardized <- data_valid
colnames(data_standardized) <-
  gsub(" ", "_", colnames(data_standardized)) #Source code : https://statisticsglobe.com/replace-spaces-in-column-names-r

#c) Data Quality Assurance:
# Check for missing values in a data frame
missing_values <-
  data_standardized %>% summarise_all( ~ sum(is.na(.)))
missing_values
# Remove rows with missing values
data_cleaned <- na.omit(data_standardized)
#d) Data Formatting: Format 'Year' column to Date format
data_cleaned$Year <- as.Date(paste0(data_cleaned$Year, "-07-01"))
# Convert certain columns to factor
cols_to_factor <-
  c(
    "Job_Family_Code",
    "Job_Code",
    "Year_Type",
    "Organization_Group",
    "Department_Code",
    "Department",
    "Union",
    "Job_Family",
    "Job"
  )
data_cleaned[cols_to_factor] <-
  lapply(data_cleaned[cols_to_factor], as.factor)
#e) Derivation of New Insights: Calculate 'Efficiency Ratio'
data_cleaned$Efficiency_Ratio <-
  data_cleaned$Total_Compensation / data_cleaned$Total_Salary
# Step 3: Data Checking
# Methods: Rigorous data checking methods
# Errors and Corrections:
# Error 1: Duplicate Entries
data_checked_duplicates_removed <-
  data_cleaned %>% distinct()  # Remove duplicate entries
# Error 2: Outliers
# Identify and correct outliers using domain knowledge
outlier_threshold <- 2
data_checked_no_outliers <- data_checked_duplicates_removed %>%
  filter(abs(scale(Total_Compensation)) < outlier_threshold)
# Visualizations:
# Box Plots to identify outliers in numerical variables
boxplot(Total_Compensation ~ Year, data = data_checked_no_outliers)
# Histograms to understand the distribution of variables
hist(data_checked_no_outliers$Efficiency_Ratio, main = "Efficiency Ratio Distribution")
#final output
str(data_checked_no_outliers)
# Extract unique departments
Num_Job_Family <- unique(data_standardized$Job_Family)
# Extract unique Job Family
Num_Departments <- unique(data_standardized$Department)
# Research Question 3: Distribution of Efficiency Ratio
# Filter out rows with zero salaries or zero compensation
data_filtered <- data_checked_no_outliers %>% filter(Salaries != 0 & (Total_Compensation - Total_Benefits) != 0)
#......................................................................................................................................................................................

my_theme <- bs_theme(
  primary = "#cceeff",  # colour selection _ Faint blue color for the header
  font_family = "Inter, sans-serif",  
  font_size = 13,  # Setting the base font size
  line_height = 1,  # Set the base line height
  navbar_color = "#cceeff",  # Matching the navbar color with the header color
  navbar_dark = TRUE,  # Use a light navbar
  navbar_text = "bold",  # Make the navbar text bold
  body_bg = "#f8f9fa",  # Set the background color for the body
  body_color = "#212529"  # Set the text color for the body
)
# UI for the Shiny app
ui <- fluidPage(
  theme = my_theme,
  shinythemes::themeSelector(),
  useShinyjs(),  # Initialize shinyjs
  navbarPage("Multi-Page App",
             tabPanel("Click HERE to view Page 1",
                      h2("Title: An Analysis of Employee Compensation Trends in the City of San Francisco"),
                      fluidRow(
                        column(3, h2("Data source"), p("San Francisco Controller's Office  https://catalog.data.gov/dataset/employee-compensation")),
                        column(9, h2("Objective / Aim"), p("⦁	To showcase the evolving Employee Compensation Trends to Economists, Human Resource Personnels, young professionals aspiring to settle in San Francisco. The primary aim of this data story is to provide a comprehensive and insightful analysis of the pay dynamics within the city's administration. By leveraging a detailed dataset spanning from 2017, sourced from the San Francisco Controller's Office, we aim to uncover valuable information about the city's organizational structure and the overall wellbeing of its personnel."))),
                      fluidRow(
                        column(2, h2("Features"), p("Inteactive charts with insights
                                                    Comprehensive
                                                    data-driven.")),
                        column(10, h2("Inspiration"), p("

⦁	Understanding Organizational Dynamics: This analysis can reveal trends, patterns, and potential areas for improvement in the organizational structure specially for HR and Economists
   
⦁	Identifying Inequalities: The data story aims to shed light on any disparities or inequalities in pay among different segments of the workforce by analysing statistical graphs using mean, median and trend    
⦁	Spotlighting Well-Paid Job Families and Departments: Through this analysis, we aspire to identify job families or departments that receive exceptional compensation. 
   
⦁	Personal Connection to San Francisco: My personal connection to San Francisco serves as a strong motivator for this analysis. 
"))),
                      mainPanel(
                        h2("Mean Salary Visualization By Department and Job Families"),
                        fluidRow(
                          column(12, plotlyOutput("departmentPlot")),
                          column(12, plotlyOutput("jobFamilyPlot"))),
                        fluidRow(
                          column(6, numericInput("numFilter", "TOP N: Enter the number to filter:", value = 10)),
                          column(6, checkboxInput("toggleFilter", "Activate Filter", value = 10)))),  # Default state of the activate button is oN
             ),
             
             tabPanel("Click HERE to view Page 2",
                      h2("Explore Compensation Trends"),
                      p("This is the second page of the app."),
                      plotlyOutput("avg_compensation_plot"),
                      plotlyOutput("top_departments_plot"),
                      fluidRow(
                        column(4, selectInput(
                          inputId = "N",
                          label = "Top N Job Department",
                          choices = 1:54,
                          selected = 10  # Set the default value to 1
                        )),
                        column(4, sliderInput(
                          inputId = "filter_year",
                          label = "Filter Year",
                          min = 2017,
                          max = 2019,
                          value = 2017,
                          step = 1)))),
             
             tabPanel("Click HERE to view Page 3", 
                      h2("Welcome to Page 3"),
                      p("This is the third page of the app."),
                      # Add UI components specific to Page 3 here
                      tags$head(
                        tags$style(HTML('
                 #plot {
                   width: 100% !important;
                 }'))),
                      fluidRow(
                        column(12, 
                               selectInput("plotType", "Select Plot Type:", choices = c("By Department", "By Job Family", "By Union")))),
                      mainPanel(
                        plotlyOutput("plot", width = "100%", height = "500px"))
             )
  )
)

# Define server
server <- function(input, output) {
  # Keeping a flag to determine if the filter has been applied
  filter_applied <- reactiveVal(FALSE)  # Default state of the button is off
  
  observe({
    if (input$toggleFilter) {
      shinyjs::enable("numFilter")
      filter_applied(TRUE)
    } else {
      shinyjs::disable("numFilter")
      filter_applied(FALSE)}})
  
  # Plot mean salary by department with tooltip
  output$departmentPlot <- renderPlotly({
    mean_salary_by_department <- data_standardized %>%
      group_by(Department) %>%
      summarize(Mean_Salary = mean(Salaries)) %>%
      arrange(desc(Mean_Salary))
    
    if (filter_applied()) {
      mean_salary_by_department <- mean_salary_by_department %>% top_n(input$numFilter)}
    
    gg <- ggplot(mean_salary_by_department, aes(x = Mean_Salary, y = reorder(Department, -Mean_Salary), fill = Mean_Salary, text = paste("Department: ", Department, "<br>Mean Salary: $", Mean_Salary))) +
      geom_bar(stat = "identity") +
      scale_fill_gradientn(colors = rainbow(length(mean_salary_by_department$Mean_Salary))) +
      labs(x = "Mean Salaries", y = "Department", title = "Top N Departments by Mean Salary") +
      theme(axis.text.y = element_text(angle = 0, hjust = 1),
            legend.position = "none")
    
    ggplotly(gg, tooltip = "text")})
  
  # Plot mean salary by job family with tooltip
  output$jobFamilyPlot <- renderPlotly({
    mean_salary_by_job_family <- data_standardized %>%
      group_by(Job_Family) %>%
      summarize(Mean_Salary = mean(Salaries)) %>%
      arrange(desc(Mean_Salary))
    
    if (filter_applied()) {
      mean_salary_by_job_family <- mean_salary_by_job_family %>% top_n(input$numFilter)
    }
    
    gg <- ggplot(mean_salary_by_job_family, aes(x = Mean_Salary, y = reorder(Job_Family, -Mean_Salary), fill = Mean_Salary, text = paste("Job Family: ", Job_Family, "<br>Mean Salary: $", Mean_Salary))) +
      geom_bar(stat = "identity") +
      scale_fill_gradientn(colors = rainbow(length(mean_salary_by_job_family$Mean_Salary))) +
      labs(x = "Mean Salaries", y = "Job_Family", title = "Top N Job Families by Mean Salary") +
      theme(axis.text.y = element_text(angle = 0, hjust = 1),
            legend.position = "none"
      )
    
    ggplotly(gg, tooltip = "text")
  })
  
  
  # Visual 1: Average Total Compensation Trend
  avg_compensation_year <- data_checked_no_outliers %>%
    group_by(Year) %>%
    summarise(Avg_Compensation = mean(Total_Compensation, na.rm = TRUE))
  
  filtered_data <- reactive({
    avg_compensation_year %>%
      filter(Year >= input$filter_year)})
  
  output$avg_compensation_plot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = Year, y = Avg_Compensation, text = paste("Year: ", Year, "<br>Avg Compensation: $", Avg_Compensation))) +
      geom_line(aes(group = 1, color = "Average Total Compensation"), size = 1.5) +
      geom_point(aes(color = "Data Points"), size = 3) +
      labs(title = "Average Total Compensation Trend", x = "Year", y = "Average Total Compensation") +
      scale_color_manual(values = c("blue", "red"),
                         breaks = c("Average Total Compensation", "Data Points"),
                         labels = c("Average Total Compensation", "Data Points")) +
      theme_minimal()
    ggplotly(gg, tooltip = "text")})
  
  observe({selected_N <- as.numeric(input$N)
  
  # Sorting data_checked_no_outliers by Avg_Compensation
  sorted_data <- data_checked_no_outliers[order(-data_checked_no_outliers$Total_Compensation), ]
  
  # Selecting the top N departments
  filtered_top_departments <- head(sorted_data, selected_N)
  
  output$top_departments_plot <- renderPlotly({
    my_palette <- viridis::viridis_pal(option = "turbo")(selected_N)
    
    gg <- ggplot(filtered_top_departments, aes(x = reorder(Department, -Total_Compensation), y = Total_Compensation, fill = Department, text = paste("Department: ", Department, "<br>Avg Compensation: $", Total_Compensation))) +
      geom_bar(stat = "identity") +
      labs(title = paste("Top", selected_N, "Departments with Highest Average Compensation"),
           x = "Department", y = "Average Compensation") +
      scale_fill_manual(values = my_palette) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      theme(axis.text.x = element_text(angle = 40, hjust = 1),
            text = element_text(size = 13))
    ggplotly(gg, tooltip = "text")})})
  
  # Assuming you have a data frame named data_filtered
  
  # Distribution across departments
  output$department_boxplot <- renderPlot({
    ggplot(data_filtered, aes(x = reorder(Department, Efficiency_Ratio), y = Efficiency_Ratio)) +
      geom_boxplot() +
      coord_flip() +
      labs(title = "Distribution of Efficiency Ratio across Departments", x = "Department", y = "Efficiency Ratio") +
      theme_minimal()
  })
  
  # Distribution across job families
  output$job_family_boxplot <- renderPlot({
    ggplot(data_filtered, aes(x = reorder(Job_Family, Efficiency_Ratio), y = Efficiency_Ratio)) +
      geom_boxplot() +
      coord_flip() +
      labs(title = "Distribution of Efficiency Ratio across Job Families", x = "Job Family", y = "Efficiency Ratio") +
      theme_minimal()
  })
  output$plot <- renderPlotly({
    filtered_data <- data_filtered
    
    if (!is.null(input$topn) && input$topn > 0) {
      filtered_data <- filtered_data %>%
        arrange(Efficiency_Ratio) %>%
        slice(1:input$topn)
    }
    
    if (input$plotType == "By Department") {
      # Create a violin plot for Efficiency Ratio by Department using plotly
      plot_ly(filtered_data, x = ~reorder(Department, Efficiency_Ratio), y = ~Efficiency_Ratio, 
              type = 'violin', box = list(visible = TRUE), marker = list(line = list(width = 2)), height = 1000) %>%
        layout(xaxis = list(tickangle = 40), showlegend = FALSE) %>%
        config(displayModeBar = TRUE)  
    } else if (input$plotType == "BDy Job Family") {
      # Create a violin plot for Efficiency Ratio by Job Family using plotly
      plot_ly(filtered_data, x = ~Job_Family, y = ~Efficiency_Ratio, 
              type = 'violin', box = list(visible = TRUE), marker = list(line = list(width = 2)), height = 1000) %>%
        layout(xaxis = list(tickangle = 40), showlegend = FALSE) %>%
        config(displayModeBar = TRUE)  
    } else if (input$plotType == "By Union") {
      # Create a violin plot for Efficiency Ratio by Union using plotly
      plot_ly(filtered_data, x = ~reorder(Union, Efficiency_Ratio), y = ~Efficiency_Ratio, 
              type = 'violin', box = list(visible = TRUE), marker = list(line = list(width = 2)), height = 1000) %>%
        layout(xaxis = list(tickangle = 40), showlegend = FALSE) %>%
        config(displayModeBar = TRUE)  
    }
  })}


# Run the app
shinyApp(ui = ui, server = server)






