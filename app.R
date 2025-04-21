library(shiny)
library(tidyverse)
library(DT)
library(bslib)

# Load dataset
alz <- read.csv("alzheimers_disease_data.csv")

# Recode variables
alz$Gender <- factor(alz$Gender, levels = c(0, 1), labels = c("Male", "Female"))

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  tags$head(
    tags$style(HTML("
      body {
        background-color: #1e1e1e !important;
        color: #f5f5f5;
      }
      .well {
        background-color: #121212 !important;
        border: none;
        border-radius: 8px;
        box-shadow: 2px 2px 8px rgba(0,0,0,0.5);
      }
      .main-panel {
        background-color: #121212;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 2px 2px 12px rgba(0,0,0,0.4);
      }
      .shiny-plot-output {
        background-color: #1e1e1e !important;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 20px;
      }
      .selectize-input, .form-control {
        background-color: #2a2a2a !important;
        color: #f5f5f5 !important;
        border: 1px solid #444 !important;
      }
      .selectize-dropdown-content .option.active {
        background-color: #4169e1 !important;
        color: white !important;
      }
      table.dataTable thead {
        background-color: #4169e1 !important;
        color: #ffffff !important;
        font-weight: 600;
        font-size: 14px;
        letter-spacing: 0.5px;
        text-align: left !important;
      }
      table.dataTable td {
        padding: 10px 16px !important;
        font-size: 13px;
        color: #f5f5f5;
        text-align: left !important;
      }
      table.dataTable tbody tr:nth-child(odd) {
        background-color: #252525 !important;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: #1a1a1a !important;
      }
      .btn-primary {
        background-color: #e91e63 !important;
        border-color: #e91e63 !important;
        color: #ffffff !important;
        font-weight: 600;
        padding: 8px 16px;
        border-radius: 6px;
      }
      .btn-primary:hover {
        background-color: #f06292 !important;
        border-color: #f06292 !important;
        box-shadow: 0 0 10px rgba(233, 30, 99, 0.5);
      }
      .shiny-input-container {
        margin-bottom: 20px;
      }
    "))
  ),
  
  titlePanel("Alzheimer's Diagnosis Explorer"),
  
  fluidRow(
    column(
      width = 12,
      wellPanel(
        HTML("<h4>About Alzheimer's Disease</h4>
          <p>Alzheimer’s disease is a progressive neurological disorder that causes memory loss, cognitive decline, and personality changes. 
          It is the most common form of dementia, affecting millions of individuals and their families worldwide. Though research continues, there is currently no cure for Alzheimer's.</p>
          <h5>How to Use This App</h5>
          <p>Select a variable from the dropdown menu to explore how it relates to Alzheimer's diagnoses. 
          Choose a chart type based on the data, and optionally facet the results by diagnosis category. 
          You can also download a summary table of the data below the chart. Click <a href='https://www.kaggle.com/datasets/rabieelkharoua/alzheimers-disease-dataset' target='_blank'>here</a> to download the full dataset. </p>")
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Choose a variable:", choices = names(alz)),
      uiOutput("chartTypeUI"),
      checkboxInput("facet", "Facet by Diagnosis", value = FALSE),
      width = 3
    ),
    
    mainPanel(
      div(class = "main-panel",
          div(class = "shiny-plot-output",
              plotOutput("mainPlot")
          ),
          br(),
          textOutput("varDescription"),
          br(),
          downloadButton("downloadData", "Download CSV", class = "btn-primary"),
          br(), br(),
          DTOutput("summaryTable")
      ),
      width = 9
    )
  )
)

# Safe fallback for missing descriptions
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Define server logic
server <- function(input, output) {
  # Descriptions for variables
  descriptions <- list(
    BMI = "Body Mass Index is the ratio of height to weight. It is a measure of fitness, but can sometimes be inaccurate for individuals who have high muscle mass.",
    Age = "Age is recorded in years at the time of data collection.",
    Gender = "Gender is recorded as Male or Female.",
    Diagnosis = "Diagnosis indicates the cognitive status of the individual: Cognitively Normal, Mild Cognitive Impairment, or Alzheimer's Disease.",
    Education = "Years of education completed by the individual.",
    SES = "Socioeconomic Status (SES) ranges from 1 (low) to 5 (high), representing social and economic standing.",
    MMSE = "Mini-Mental State Examination (MMSE) is a common measure of cognitive function, scored from 0 to 30.",
    eTIV = "Estimated Total Intracranial Volume (eTIV) is a measure of brain volume from MRI scans.",
    nWBV = "Normalized Whole Brain Volume (nWBV) is a measure of total brain size adjusted for head size.",
    ASF = "Atlas Scaling Factor (ASF) is used in MRI preprocessing to normalize head sizes across individuals."
  )
  
  output$chartTypeUI <- renderUI({
    req(input$var)
    var <- alz[[input$var]]
    
    if (is.numeric(var)) {
      selectInput("chart", "Chart Type:", choices = c("Histogram", "Box Plot"))
    } else if (is.factor(var) || is.character(var)) {
      selectInput("chart", "Chart Type:", choices = c("Bar Chart"))
    } else {
      selectInput("chart", "Chart Type:", choices = c("Unsupported"))
    }
  })
  
  output$mainPlot <- renderPlot({
    req(input$var, input$chart)
    var <- alz[[input$var]]
    
    if (input$chart == "Histogram") {
      p <- ggplot(alz, aes_string(x = input$var)) +
        geom_histogram(fill = "#00ffd0", bins = 30, alpha = 0.85, color = "#121212") +
        labs(title = paste("Histogram of", input$var), x = input$var, y = "Count")
      if (input$facet) p <- p + facet_wrap(~ Diagnosis)
      p
      
    } else if (input$chart == "Bar Chart") {
      p <- ggplot(alz, aes_string(x = input$var)) +
        geom_bar(fill = "#00ffd0", alpha = 0.85) +
        labs(title = paste("Bar Chart of", input$var), x = input$var, y = "Count")
      if (input$facet) p <- p + facet_wrap(~ Diagnosis)
      p
      
    } else if (input$chart == "Box Plot") {
      p <- ggplot(alz, aes_string(x = if (input$facet) "factor(Diagnosis)" else NULL, y = input$var)) +
        geom_boxplot(fill = "#e91e63", alpha = 0.7, color = "#121212") +
        labs(
          title = paste("Box Plot of", input$var),
          x = if (input$facet) "Diagnosis" else "",
          y = input$var
        )
      p
    }
  })
  
  output$varDescription <- renderText({
    req(input$var)
    descriptions[[input$var]] %||% "No description available for this variable."
  })
  
  output$summaryTable <- renderDT({
    if (input$facet) {
      summary_data <- alz %>%
        group_by(.data[[input$var]], Diagnosis) %>%
        summarize(Count = n(), .groups = "drop")
    } else {
      summary_data <- alz %>%
        group_by(.data[[input$var]]) %>%
        summarize(Count = n(), .groups = "drop")
    }
    
    datatable(
      summary_data,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'ftip',
        ordering = TRUE
      ),
      filter = "top",
      rownames = FALSE,
      class = "display nowrap compact",
      style = "bootstrap"
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("alzheimers_summary_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      summary_data <- alz %>%
        group_by(.data[[input$var]]) %>%
        summarize(Count = n(), .groups = "drop")
      write.csv(summary_data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
