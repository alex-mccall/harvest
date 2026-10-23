library(shiny)
library(harvest)

ui <- fluidPage(
  titlePanel("Scottish Metadata Exchange Discovery"),
  sidebarLayout(
    sidebarPanel(
      textInput("query", "Search", value = "health"),
      actionButton("run", "Search")
    ),
    mainPanel(
      tableOutput("results")
    )
  )
)

server <- function(input, output, session) {
  sample_data <- reactiveVal(
    dplyr::bind_rows(
      harvest:::canonical_metadata_row(
        dataset_id = "health-1",
        dataset_name = "Health indicators",
        description = "Public health indicators for Scotland.",
        source_catalogue = "Example catalogue",
        organisation = "NHS Scotland",
        keywords = c("health", "indicators"),
        themes = c("health"),
        quality_score = 90
      ),
      harvest:::canonical_metadata_row(
        dataset_id = "transport-1",
        dataset_name = "Transport usage",
        description = "Transport usage statistics.",
        source_catalogue = "Example catalogue",
        organisation = "Transport Scotland",
        keywords = c("transport"),
        themes = c("transport"),
        quality_score = 82
      )
    )
  )

  results <- eventReactive(input$run, {
    search_catalogue(sample_data(), input$query)
  })

  output$results <- renderTable({
    results()[, c(
      "dataset_id",
      "dataset_name",
      "source_catalogue",
      "quality_score"
    )]
  })
}

shinyApp(ui, server)
