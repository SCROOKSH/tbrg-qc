# Cumulative rainfall plot Shiny App
# This script is an RShiny App.
# Please run this after running the *03_examine.R* script, which generates the `df_wide.Rda` data that this Shiny App uses.

# Version: 0.1
# Last update: 2023-03-13
library(shiny)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(plotly)
library(viridisLite)
library(viridis)

# Load data
load("../03_examine/df_wide.Rda")

# Define date limits
slider1 <- min(as.Date(df_wide$Timestamp)-5)
slider2 <- max(as.Date(df_wide$Timestamp)+5)

# Define UI
ui <- navbarPage(title = "Tipping rain budget data QAQC", theme = "spacelab",
                 
                 tabPanel(title = "Cumulative rain plot",
                          br(),
                          fluidRow(
                            column(width = 12, 
                                   plotlyOutput("plot", height = 600))
                          ),
                          
                          fluidRow(
                            column(width = 12,
                                   sliderInput("date_slider", "Select Date Range",
                                               min = slider1, max = slider2,
                                               value = c(slider1, slider2),
                                               timeFormat = "%Y-%m-%d",
                                               width="90%"))
                          )
                 ),
                 tabPanel(title = "Documentation",
                 fluidPage(
                   tags$iframe(src = './documentation.html',         # put myMarkdown.html to ~/www/
                               width = '100%', height = '854px', 
                               frameborder = 0, scrolling = 'auto'
                   )
                 )
))

# Define server logic
server <- function(input, output) {
  
  # Process data based on user action on the date slider
  data_plot <- reactive({
    df_wide %>%
      filter(Timestamp >= input$date_slider[1] & Timestamp <= input$date_slider[2]) %>% 
      arrange(Timestamp) %>%
      mutate_at(vars(starts_with("Rain_")), ~replace_na(., 0)) %>%
      mutate(across(starts_with("Rain_"), ~ cumsum(.), .names = "CDF_{.col}"))
  })
  
  # Create plot
  output$plot <- renderPlotly({
    
    # Filter columns that start with "CDF_"
    cdf_long <- data_plot() %>%
      select(- starts_with("Rain_")) %>%
      pivot_longer(cols = starts_with("CDF_"), 
                   names_to = "CDF_col", 
                   values_to = "CDF_value")
    
    p <- ggplot(cdf_long, aes(x = Timestamp, y = CDF_value, color = CDF_col)) +
      geom_line() +
      labs(y = "Cumulative Rainfall (mm)", x = "") +
      theme_bw() +
      scale_x_datetime(date_breaks = "1 week", date_labels = "%b-%d") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
            legend.background = element_rect(fill = "transparent")) +
      labs(color = NULL) +
      scale_color_viridis(discrete=TRUE)
    
    ggplotly(p)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
