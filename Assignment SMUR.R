# Load required packages
#install.packages("shiny")

library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("House Price Prediction"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = c(".csv")),
      actionButton("analyze", "Perform Analysis")
    ),
    mainPanel(
      h3("Linear Regression Results"),
      verbatimTextOutput("summary"),
      downloadButton("download_coefficients", "Download Coefficients")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$analyze, {
    req(input$file)
    data <- read.csv(input$file$datapath)
    model <- lm(Price ~ Size + Bedrooms + Bathrooms, data = data)
    output$summary <- renderPrint({
      summary(model)
    })
    output$download_coefficients <- downloadHandler(
      filename = function() { "coefficients.csv" },
      content = function(file) {
        write.csv(coef(model), file, row.names = FALSE)
      }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
