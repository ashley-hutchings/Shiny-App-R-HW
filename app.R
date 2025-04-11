
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/


library(shiny)
library(tidyverse)
library(DT)

#Load dataset
alz <- read.csv("alzheimers_disease_data.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Alzheimer's Diagnosis Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("binVar", "Choose a variable to bin:",
                  choices = c("DietQuality", "SleepQuality"),
                  selected = "DietQuality")
    ),
    
    mainPanel(
      plotOutput("barPlot"),
      DTOutput("summaryTable")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Create binned version of selected variable
  binned_data <- reactive({
    bin_var <- alz[[input$binVar]]
    bins <- cut(bin_var,
                breaks = quantile(bin_var, probs = seq(0, 1, 0.2), na.rm = TRUE),
                include.lowest = TRUE,
                labels = c("Very Low", "Low", "Medium", "High", "Very High"))
    
    df <- alz
    df$BinnedVar <- bins
    df
  })
  
  # Create bar plot
  output$barPlot <- renderPlot({
    ggplot(binned_data(), aes(x = BinnedVar, fill = factor(Diagnosis))) +
      geom_bar(position = "dodge") +
      labs(title = paste("Diagnosis Count by", input$binVar),
           x = paste(input$binVar, "(Binned)"),
           fill = "Diagnosis") +
      theme_minimal()
  })
  
  # Create summary table
  output$summaryTable <- renderDT({
    binned_data() %>%
      group_by(BinnedVar, Diagnosis) %>%
      summarize(Count = n(), .groups = "drop") %>%
      datatable()
  })
}
# Run  app
shinyApp(ui = ui, server = server)
