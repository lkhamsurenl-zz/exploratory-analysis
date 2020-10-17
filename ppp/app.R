library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyFeedback)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(glue)

CONCEPTS_FILENAME <- "~/development/exploratory-analysis/risk_factor/concepts.csv"
MEASUREMENT_REFERENCE_RANGE_FILENAME = "~/development/exploratory-analysis/risk_factor/measurement_reference_range.csv"
MEASUREMENTS_FILENAME <- "measurements.csv"

VITALS <- c("Blood Pressure", "Body Temperature", "Height", "Respiratory Rate", "Heart Rate", "Weight", "Body mass index")
VITALS_REGEX <- paste0("(", paste(tolower(VITALS), collapse = '|'), ")")

ui <- function(request) {
  fluidPage(
    theme = shinytheme("paper"),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    titlePanel("PPP"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("vitalSignOptions", "Vital Signs", VITALS),
        actionButton("add", "Add new measurement")
      ),
      mainPanel(
        tabsetPanel(
          id = "tabs",
          type = "hidden",
          tabPanel(
            "Vitals",
            value = "vitals", 
            plotOutput("vitalsPlot")
          ),
          tabPanel("Labs", value = "labs")
        )
      )
    )
  )
}

server <- function(input, output, session) {
  dat <- reactiveValues(
    concepts = read_csv(CONCEPTS_FILENAME, col_types = cols(concept_id = "c")) %>% 
      filter(str_detect(tolower(concept_name), VITALS_REGEX)),
    measurementReferenceRanges = read_csv(MEASUREMENT_REFERENCE_RANGE_FILENAME, col_types = cols(concept_id = "c")),
    measurements = read_csv(MEASUREMENTS_FILENAME, col_types = cols(concept_id = "c"))
  )
  
  measurementConcepts <- reactive(
    dat$measurements %>% 
      inner_join(dat$concepts, by = c("concept_id")) %>% 
      left_join(dat$measurementReferenceRanges, by = c("concept_id"))
  )
  
  # observeEvent(input$vitalSignOptions, {
  #   if (input$vitalSignOptions == "Blood Pressure") {
  #     updateTabsetPanel(session, "tabs", selected = "bloodPressure")
  #   }
  # })
  
  ### Add new measurement
  observeEvent(input$add, {
    showModal(modalDialog(
      title = "Add new value",
      textInput("userId", "User Name"),
      dateInput("date", "Date", format = "yyyy-mm-dd"),
      tags$div(id = 'placeholder'),
      actionButton("new", "New"),
      hr(),
      actionButton("save", "Save")
    ))
  })
  
  inserted <- c()
  observeEvent(input$new, {
    btn <- input$new
    insertUI(
      selector = '#placeholder',
      ui = fluidRow(
        column(6, selectInput(glue("conceptNameId{btn}"), "Concept", choices = glue("{dat$concepts$concept_name} ({dat$concepts$concept_id})"))),
        column(2, numericInput(glue("value{btn}"), "Value", value = 0))
      )
    )
    inserted <<- c(btn, inserted)
  })
  
  observeEvent(input$save, {
    req(input$userId, input$date)
    
    for (i in 1:inserted) {
      req(input[[glue("conceptNameId{i}")]], input[[glue("value{i}")]])
      
      dat$measurements <- dat$measurements %>% 
        add_row(
          user_id = input$userId, 
          concept_id = sub(".+ \\(([0-9]+)\\)", "\\1", input[[glue("conceptNameId{i}")]]), 
          value = input[[glue("value{i}")]],
          date = input$date
        )
    }
    dat$measurements %>% 
      write_csv(MEASUREMENTS_FILENAME)
    
    shinyjs::reset("value")
    shinyjs::reset("date")
  })
  
  ### Vitals tab
  output$vitalsPlot <- renderPlot({
      measurementConcepts() %>% 
        filter(str_detect(tolower(concept_name), tolower(input$vitalSignOptions))) %>% 
        ggplot(aes(x = date, y = value, color = concept_name)) + 
        geom_line() + 
        geom_point() + 
        geom_rect(aes(ymin = low_range, ymax = high_range), color = NA, alpha = 0.03, fill = "palegreen", xmin = -Inf, xmax = Inf) +
        labs(
          title = input$vitalSignOptions,
          x = "Measurement date",
          y = "Value"
        )
  })
}

shinyApp(ui = ui, server = server)