library(shiny)
library(tidyverse)
library(DT)
library(bslib)

# Load dataset
alz <- read.csv("alzheimers_disease_data.csv")

# Define UI
ui <- fluidPage(
  theme = bs_theme(base_font = font_google("Inter"), bootswatch = "darkly"),
  
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

      table.dataTable thead th {
        padding: 10px 16px !important;
        text-align: left !important;
      }

      table.dataTable td {
        padding: 10px 16px !important;
        font-size: 13px;
        color: #f5f5f5;
        text-align: left !important;
      }

      table.dataTable tbody tr {
        line-height: 1.5;
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
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "binVar",
        label = "Choose a variable to bin:",
        choices = c("DietQuality", "SleepQuality"),
        selected = "DietQuality"
      ),
      width = 3
    ),
    
    mainPanel(
      div(class = "main-panel",
          div(class = "shiny-plot-output",
              plotOutput("barPlot")
          ),
          downloadButton("downloadData", "Download CSV", class = "btn-primary"),
          br(), br(),
          DTOutput("summaryTable")
      ),
      width = 9
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  binned_data <- reactive({
    bin_var <- alz[[input$binVar]]
    bins <- cut(
      bin_var,
      breaks = quantile(bin_var, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Very Low", "Low", "Medium", "High", "Very High")
    )
    
    df <- alz
    df$BinnedVar <- bins
    df
  })
  
  output$barPlot <- renderPlot({
    ggplot(binned_data(), aes(x = BinnedVar, fill = factor(Diagnosis))) +
      geom_bar(
        position = "dodge",
        color = "#121212",
        size = 0.3,
        alpha = 0.85
      ) +
      scale_fill_manual(values = c("0" = "#00ffd0", "1" = "#e91e63")) +
      labs(
        title = paste("Diagnosis Count by", input$binVar),
        x = paste(input$binVar, "(Binned)"),
        fill = "Diagnosis"
      ) +
      theme_minimal(base_family = "Inter") +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#f5f5f5"),
        axis.title = element_text(size = 14, color = "#f5f5f5"),
        axis.text = element_text(size = 12, color = "#cccccc"),
        legend.title = element_text(size = 13, color = "#f5f5f5"),
        legend.text = element_text(size = 11, color = "#cccccc"),
        panel.background = element_rect(fill = "#1e1e1e", color = NA),
        plot.background = element_rect(fill = "#1e1e1e", color = NA),
        panel.grid.major = element_line(color = "#333333"),
        panel.grid.minor = element_blank()
      )
  })
  
  summary_data <- reactive({
    binned_data() %>%
      group_by(BinnedVar, Diagnosis) %>%
      summarize(Count = n(), .groups = "drop")
  })
  
  output$summaryTable <- renderDT({
    datatable(
      summary_data(),
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = "400px",  # ← THIS prevents extra space
        dom = 'ftip',       # omit buttons to simplify layout
        ordering = TRUE
      ),
      filter = "top",
      rownames = FALSE,
      class = "display nowrap compact",
      style = "bootstrap"
    )
  })
  
  
  
  
  # output$summaryTable <- renderDT({
  #   datatable(
  #     binned_data() %>%
  #       group_by(BinnedVar, Diagnosis) %>%
  #       summarize(Count = n(), .groups = "drop"),
  #     options = list(
  #       pageLength = 10,
  #       autoWidth = TRUE,
  #       scrollX = TRUE,
  #       dom = 'Bfrtip',
  #       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
  #       initComplete = JS(
  #         "function(settings, json) {",
  #         "$(this.api().table().header()).css({'color': '#fff'});",
  #         "}")
  #     ),
  #     extensions = 'Buttons',
  #     filter = 'top',
  #     rownames = FALSE,
  #     class = "display nowrap compact",
  #     style = "bootstrap"
  #   )
  # })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("alzheimers_summary_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        binned_data() %>%
          group_by(BinnedVar, Diagnosis) %>%
          summarize(Count = n(), .groups = "drop"),
        file,
        row.names = FALSE
      )
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
