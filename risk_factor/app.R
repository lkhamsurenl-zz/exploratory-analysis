library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyFeedback)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(glue)

CONCEPTS_FILENAME <- "concepts.csv"
RISK_FACTORS_FILENAME <- "risk_factors.csv"

jscode <- '
var down = {};
$(document).keydown(function(e) {
  down[e.keyCode] = true;
}).keyup(function(e) {
if (down[13] && down[91]) {
  $("#save").click()
}
  down[e.keyCode] = false;
});
'

ui <- function(request) {
  fluidPage(
    theme = shinytheme("cerulean"),
    tags$head(tags$script(HTML(jscode))),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    titlePanel("Non communicable disease risk factors"),
    sidebarLayout(
      sidebarPanel(
        # in side bar, all input happens
        radioButtons("type", "Concept or Risk Factor", c("concepts", "risk-factors")),
        hr(),
        uiOutput("inputs"),
        hr(),
        actionButton("save", "Save", class="btn-success")
      ),
      
      # Show currently selected CSV file
      mainPanel(
        dataTableOutput("csv")
      )
    )
  )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  dat <- reactiveValues(
    concepts = read_csv(CONCEPTS_FILENAME, col_types = cols(concept_id = "c")) %>% 
      mutate(display_name = glue("{concept_name} ({concept_id})")),
    riskFactors = read_csv(RISK_FACTORS_FILENAME, col_types = cols(condition_concept_id = "c", risk_concept_id = "c"))
  )
  
  output$inputs <- renderUI({
    if (input$type == "concepts") {
      div(
        textInput("conceptId", "Concept ID"),
        textInput("conceptName", "Concept Name")
      )
    } else {
      div(
        selectInput("conditionConcept", "Condition Concept", choices = dat$concepts$display_name),
        textInput("url", "Source URL"),
        hr(),
        selectInput("riskConcepts", "Risk Factor Concepts", choices = dat$concepts$display_name, multiple = TRUE)
      )
    }
  })
  
  output$csv <- renderDataTable({
    if (input$type == "concepts") {
      dat$concepts
    } else {
      dat$riskFactors
    }
  })
  
  # save output, depending on type
  observeEvent(input$save, {
    if (input$type == "concepts") {
      req(input$conceptId, input$conceptName)
      # validate concept id
      isConceptIdNumeric <- grepl("^[0-9]+$", input$conceptId, perl = T)
      shinyFeedback::feedbackDanger("conceptId", !isConceptIdNumeric, "Concept is not valid numeric!")
      req(isConceptIdNumeric)
      
      isConceptIdAlreadyExists <- input$conceptId %in% dat$concepts$concept_id
      shinyFeedback::feedbackDanger("conceptId", isConceptIdAlreadyExists, "Concept is already added!")
      req(!isConceptIdAlreadyExists)
      
      dat$concepts <- dat$concepts %>% 
        add_row(
          concept_id = input$conceptId, 
          concept_name = input$conceptName, 
          display_name = glue("{input$conceptName} ({input$conceptId})")
        )
      
      dat$concepts %>% 
        select(concept_id, concept_name) %>% 
        write_csv(CONCEPTS_FILENAME)
      
      # clear the input
      shinyjs::reset("conceptId")
      shinyjs::reset("conceptName")
    } else {
      req(input$conditionConcept, input$riskConcepts, input$url)
      
      # there are list of risk concepts listed for same condition & url
      dat$riskFactors <- dat$riskFactors %>% 
        add_row(
          condition_concept_id = extractConceptId(input$conditionConcept), 
          risk_concept_id = extractConceptId(input$riskConcepts), 
          url = input$url
        )
      dat$riskFactors %>% 
        write_csv(RISK_FACTORS_FILENAME)
      
      # clear the input
      shinyjs::reset("conditionConcept")
      shinyjs::reset("riskConcepts")
      shinyjs::reset("url")
    }
  })
}

extractConceptId <- function(displayName) {
  # extract ID given display name in format: `smoking (ID)`
  sub(".+ \\(([0-9]+)\\)", "\\1", displayName)
}

# Run the application 
shinyApp(ui = ui, server = server)

