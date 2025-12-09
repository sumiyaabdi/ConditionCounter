library(shiny)

ui <- fluidPage(
  titlePanel("Condition Trends"),

  selectInput("condition",
              "Select condition:",
              choices = NULL,
              multiple = TRUE),

  checkboxInput("byMonth",
                "Plot by month",
                value = FALSE),

  numericInput("startYear", "Start Year:", value = 1901),
  numericInput("endYear", "End Year:", value = 2025),

  plotOutput("trendPlot")
)

server <- function(input, output, session) {

  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  connection <- DatabaseConnector::connect(connectionDetails)
  session$onSessionEnded(function() {
    DatabaseConnector::disconnect(connection)
  })

  occurrences <- extractPatients(connection, addNames = TRUE)

  updateSelectInput(session, "condition", choices = sort(unique(occurrences$concept_name)))

  selectedConditions <- reactive({
    req(input$condition)
    input$condition
  })

  filteredOccurrences <- reactive({
    if (!is.null(input$startYear) && !is.na(input$startYear)) {
      occurrences <- occurrences[occurrences$year >= input$startYear, ]
    }
    if (!is.null(input$endYear) && !is.na(input$endYear)) {
      occurrences <- occurrences[occurrences$year <= input$endYear, ]
    }
    occurrences
  })

  output$trendPlot <- renderPlot({
    req(selectedConditions())
    plotTrend(filteredOccurrences(),
              byMonth = input$byMonth,
              conditionName = selectedConditions())
  })

}

shinyApp(ui, server)
