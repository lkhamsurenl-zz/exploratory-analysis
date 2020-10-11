library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyFeedback)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(glue)
library(visNetwork)

CONCEPTS_FILENAME <- "concepts.csv"
RISK_FACTORS_FILENAME <- "risk_factors.csv"

# jscode <- '
# var down = {};
# $(document).keydown(function(e) {
#   down[e.keyCode] = true;
# }).keyup(function(e) {
# if (down[13] && down[91]) {
#   $("#save").click()
# }
#   down[e.keyCode] = false;
# });
# '

ui <- function(request) {
  fluidPage(
    theme = shinytheme("paper"),
    # tags$head(tags$script(HTML(jscode))),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    titlePanel("Non communicable disease risk factors"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("inputs"),
        hr(),
        actionButton("save", "Save", class="btn-success")
      ),
      
      # Show currently selected CSV file
      mainPanel(
        tabsetPanel(
          id = "options",
          type = "tabs",
          tabPanel("Concepts", value = "concepts", dataTableOutput("conceptsCSV")),
          tabPanel("Risk Factors", value = "riskFactors", dataTableOutput("riskFactorsCSV")),
          tabPanel("Visualization", value = "visualization", visNetworkOutput("graph"))
        )
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
  
  observeEvent(input$options, {
    if (input$options == "visualization") {
      shinyjs::disable("save")
    } else {
      shinyjs::enable("save")
    }
  })
  
  output$inputs <- renderUI({
    if (input$options == "concepts") {
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
  
  ### Concepts
  output$conceptsCSV <- renderDataTable({
    dat$concepts
  })
  
  ### Risk factors 
  output$riskFactorsCSV <- renderDataTable({
    dat$riskFactors
  })
  
  # save output, depending on type
  observeEvent(input$save, {
    req(input$options %in% c("concepts", "riskFactors"))
    if (input$options == "concepts") {
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
  
  ### visualization
  output$graph <- renderVisNetwork({
    nodes <- dat$concepts %>% 
      select(concept_name) %>% 
      transmute(id = concept_name, label = concept_name)
    
    edges <- dat$riskFactors %>% 
      inner_join(dat$concepts, by = c("condition_concept_id" = "concept_id")) %>% 
      inner_join(dat$concepts, by = c("risk_concept_id" = "concept_id")) %>% 
      transmute(
        from = concept_name.y,
        to = concept_name.x
      )

    visNetwork(nodes, edges) %>% 
      visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = list(enabled = TRUE, style = "background: #f8f8f8; color: darkgreen;")
      ) %>% 
      visEdges(arrows = "to")
  })
}

extractConceptId <- function(displayName) {
  # extract ID given display name in format: `smoking (ID)`
  sub(".+ \\(([0-9]+)\\)", "\\1", displayName)
}

# Run the application 
shinyApp(ui = ui, server = server)

