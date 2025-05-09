# Load libraries
library(shiny)
library(tidyverse)
library(DT)
library(bslib)

# Load dataset
alz <- read.csv("alzheimers_disease_data.csv")

# Recode variables to factor as needed
alz$Gender <- factor(alz$Gender, levels = c(0, 1), labels = c("0: Male", "1: Female"))
alz$PatientID <- factor(alz$PatientID)
alz$Ethnicity <- factor(alz$Ethnicity, levels = c(0, 1,2,3), labels = c("0: Caucasian", "1: African American", "2: Asian", "3: Other"))
alz$EducationLevel <- factor(alz$EducationLevel, levels = c(0, 1,2,3), labels = c("0: None", "1: High School", "2: Bachelors", "3: Higher"))
alz$Diagnosis <- factor(alz$Diagnosis, levels = c(0, 1), labels = c("0: No", "1: Yes"))
alz$Smoking <- factor(alz$Smoking)
alz$FamilyHistoryAlzheimers <- factor(alz$FamilyHistoryAlzheimers)
alz$CardiovascularDisease <- factor(alz$CardiovascularDisease)
alz$Diabetes <- factor(alz$Diabetes)
alz$Depression <- factor(alz$Depression)
alz$HeadInjury <- factor(alz$HeadInjury)
alz$Hypertension <- factor(alz$Hypertension)
alz$MemoryComplaints <- factor(alz$MemoryComplaints)
alz$BehavioralProblems <- factor(alz$BehavioralProblems)
alz$Confusion <- factor(alz$Confusion)
alz$Disorientation <- factor(alz$Disorientation)
alz$PersonalityChanges <- factor(alz$PersonalityChanges)
alz$DifficultyCompletingTasks <- factor(alz$DifficultyCompletingTasks)
alz$Forgetfulness <- factor(alz$Forgetfulness)

# Define UI colors, margins, dimensions, and theme
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
      .dataTables_paginate .paginate_button {
        margin: 0 6px !important;
      }
    "))
  ),

  # Title of App  
  titlePanel("Alzheimer's Diagnosis Explorer"),

  # Subtitle section 
  fluidRow(
    column(
      width = 12,
      wellPanel(
        HTML("<h4>About Alzheimer's Disease</h4>
          <p>Alzheimer’s Disease is a progressive neurological disorder that leads to memory loss, cognitive decline, and changes in behavior or personality.
It is the most common form of dementia, impacting millions of individuals and their families around the world.
While research is ongoing, there is currently no known cure — making early detection and understanding of contributing factors more important than ever.</p>
          <h5>How to Use This App</h5>
          <p>Select a variable from the dropdown menu to explore its relationship to Alzheimer's diagnoses. Chart options will update automatically based on the data type, allowing you to choose the most appropriate visualization. 
             For deeper insight, you can optionally break down results by diagnosis category using the faceting option (default). 
             Below the chart, you’ll find a downloadable summary table for further analysis. Click <a href='https://www.kaggle.com/datasets/rabieelkharoua/alzheimers-disease-dataset' target='_blank'>here</a> to download the full dataset. </p>")
      )
    )
  ),
  
  # Sidebar/filters layout
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "Choose a variable:", choices = names(alz), selected="Gender"),
      uiOutput("chartTypeUI"),
      checkboxInput("facet", "Facet by Diagnosis", value = TRUE),
      width = 3
    ),
 
    # Layout/order of main panel   
    mainPanel(
      div(class = "main-panel",
          div(class = "shiny-plot-output",
              plotOutput("mainPlot")
          ),
          br(),
          textOutput("varDescription"),
          br(),
          tableOutput("summaryStats"),
          br(), br(),
          downloadButton("downloadData", "Download Table CSV", class = "btn-primary"),
          br(), 
          DTOutput("summaryTable")
      ),
      width = 9
    )
  )
)

# Fallback for missing descriptions
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Defined server logic
server <- function(input, output) {
  # Descriptions for variables, most are taken directly from the Kaggle website
  descriptions <- list(
    PatientID = "Unique ID for patient.",
    BMI = "Body Mass Index (BMI) is the ratio of height to weight.",
    Age = "Age is recorded in years at the time of data collection.",
    Gender = "Gender is recorded as 0-Male or 1-Female.",
    Diagnosis = "Diagnosis status for Alzheimer's Disease, where 0 indicates No and 1 indicates Yes.",
    EducationLevel = "Years of education completed by the individual.",
    Smoking = "Smoking status, where 0 indicates No and 1 indicates Yes.",
    MMSE = "Mini-Mental State Examination (MMSE) score. MMSE is a common measure of cognitive function, scored from 0 to 30, where lower scores indicate cognitive impairment.",
    AlcoholConsumption = "Weekly alcohol consumption in units.",
    SleepQuality = "Sleep Quality Score ranging from 4 to 10.",
    DietQuality = "Diet Quality Score ranging from 0 to 10.",
    FamilyHistoryAlzheimers = "Family history of Alzheimer's Disease, where 0 indicates No and 1 indicates Yes.",
    CardiovascularDisease = "Presence of cardiovascular disease, where 0 indicates No and 1 indicates Yes.",
    Diabetes = "Presence of diabetes, where 0 indicates No and 1 indicates Yes.",
    Depression = "Presence of depression, where 0 indicates No and 1 indicates Yes.",
    HeadInjury = "History of head injury, where 0 indicates No and 1 indicates Yes.",
    Hypertension = "Presence of hypertension aka high blood pressure, where 0 indicates No and 1 indicates Yes.",
    SystolicBP= "Systolic blood pressure, ranging from 90 to 180 mmHg.",
    DiastolicBP= "Diastolic blood pressure, ranging from 60 to 120 mmHg.",
    CholesterolTotal= "Total cholesterol levels, ranging from 150 to 300 mg/dL. Generally, under 200 mg/dL is healthy.",
    CholesterolLDL= "Low-density lipoprotein cholesterol levels, ranging from 50 to 200 mg/dL. Lower LDL is healthier.",
    CholesterolHDL= "High-density lipoprotein cholesterol levels, ranging from 20 to 100 mg/dL. 60 mg/dL or higher is considered healthy.",
    CholesterolTriglycerides= "Triglycerides levels, ranging from 50 to 400 mg/dL.Generally, under 150 mg/dL is healthy.",
    FunctionalAssessment= "Functional assessment score, ranging from 0 to 10. Lower scores indicate greater impairment.",
    MemoryComplaints= "Presence of memory complaints, where 0 indicates No and 1 indicates Yes.",
    BehavioralProblems= "Presence of behavioral problems, where 0 indicates No and 1 indicates Yes.",
    ADL= "Activities of Daily Living score, ranging from 0 to 10. Lower scores indicate greater impairment.",
    Confusion= "Presence of confusion, where 0 indicates No and 1 indicates Yes.",
    Disorientation= "Presence of disorientation, where 0 indicates No and 1 indicates Yes.",
    PersonalityChanges= "Presence of personality changes, where 0 indicates No and 1 indicates Yes.",
    DifficultyCompletingTasks= "Presence of difficulty completing tasks, where 0 indicates No and 1 indicates Yes.",
    Forgetfulness= "Presence of forgetfulness, where 0 indicates No and 1 indicates Yes.",
    DoctorInCharge= "This column contains confidential information about the doctor in charge, with XXXConfid as the value for all patients."
  )
  
  # Defining conditional summary stats table to only show when variable is numeric
  output$summaryStats <- renderTable({
    req(input$var)
    var <- alz[[input$var]]
    
    if (is.numeric(var)) {
      if (input$facet) {
        alz %>%
          group_by(Diagnosis) %>%
          summarize(
            Mean = round(mean(.data[[input$var]], na.rm = TRUE), 2),
            Median = round(median(.data[[input$var]], na.rm = TRUE), 2),
            SD = round(sd(.data[[input$var]], na.rm = TRUE), 2),
            Min = round(min(.data[[input$var]], na.rm = TRUE), 2),
            Max = round(max(.data[[input$var]], na.rm = TRUE), 2),
            Count = sum(!is.na(.data[[input$var]])),
            .groups = "drop"
          )
      } else {
        tibble(
          Mean = round(mean(var, na.rm = TRUE), 2),
          Median = round(median(var, na.rm = TRUE), 2),
          SD = round(sd(var, na.rm = TRUE), 2),
          Min = round(min(var, na.rm = TRUE), 2),
          Max = round(max(var, na.rm = TRUE), 2),
          Count = sum(!is.na(var))
        )
      }
    } else {
      NULL
    }
  })
  
# Defines which chart options should be available given the variable type  
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
  
# Renders the charts given the input criteria and conditions  
  output$mainPlot <- renderPlot({
    req(input$var, input$chart)
    var <- alz[[input$var]]
    
    base_theme <- theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
    
    if (input$chart == "Histogram") {
      if (input$facet) {
        p <- ggplot(alz, aes_string(x = input$var, fill = "Diagnosis")) +
          geom_histogram(bins = 30, alpha = 0.85, color = "#121212", position = "identity") +
          scale_fill_manual(values = c("0: No" = "#00ffd0", "1: Yes" = "#e91e63")) +
          facet_wrap(~ Diagnosis, labeller = labeller(Diagnosis = c("0: No" = "Diagnosis: No", "1: Yes" = "Diagnosis: Yes")))
      } else {
        p <- ggplot(alz, aes_string(x = input$var)) +
          geom_histogram(fill = "#00ffd0", bins = 30, alpha = 0.85, color = "#121212")
      }
      
      p <- p + labs(
        title = paste("Histogram of", input$var, if (input$facet) "by Diagnosis" else ""),
        x = input$var,
        y = "Count"
      ) + base_theme
      
      p
      
    } else if (input$chart == "Bar Chart") {
      if (input$facet) {
        p <- ggplot(alz, aes_string(x = input$var, fill = "Diagnosis")) +
          geom_bar(alpha = 0.85, color = "#121212", position = "dodge") +
          scale_fill_manual(values = c("0: No" = "#00ffd0", "1: Yes" = "#e91e63")) +
          facet_wrap(~ Diagnosis, labeller = labeller(Diagnosis = c("0: No" = "Diagnosis: No", "1: Yes" = "Diagnosis: Yes")))
      } else {
        p <- ggplot(alz, aes_string(x = input$var)) +
          geom_bar(fill = "#00ffd0", alpha = 0.85, color = "#121212")
      }
      
      p <- p + labs(
        title = paste("Bar Chart of", input$var, if (input$facet) "by Diagnosis" else ""),
        x = input$var,
        y = "Count"
      ) + base_theme
      
      p
      
    } else if (input$chart == "Box Plot") {
      if (input$facet) {
        p <- ggplot(alz, aes_string(
          x = "Diagnosis",
          y = input$var,
          fill = "Diagnosis"
        )) +
          geom_boxplot(alpha = 0.7, color = "#121212") +
          scale_fill_manual(values = c("0: No" = "#00ffd0", "1: Yes" = "#e91e63"))
      } else {
        p <- ggplot(alz, aes_string(
          x = "\"\"",
          y = input$var
        )) +
          geom_boxplot(fill = "#00ffd0", alpha = 0.7, color = "#121212")
      }
      
      p <- p + labs(
        title = paste("Box Plot of", input$var, if (input$facet) "by Diagnosis" else ""),
        x = if (input$facet) "Diagnosis" else "",
        y = input$var
      ) + base_theme
      
      p
    }
  })
  
  
  
 # Defines the default variable description if I have not explicitly defined it above. 
  output$varDescription <- renderText({
    req(input$var)
    descriptions[[input$var]] %||% "No description available for this variable."
  })
  
  # Renders summary data table with filters for each column
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
  
  # Renders the Download CSV button to download the summary data table
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
