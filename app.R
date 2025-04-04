# deploy with shiny
# app.R
install.packages("shiny")
library(shiny)
library(ggplot2)
library(dplyr)
install.packages("readr")
library(readr)

# Load predictions
future_data <- read_csv("predicted_species_2026.csv")
colnames(future_data)

ui <- fluidPage(
  titlePanel("2026 Predicted Foodborne Outbreak Species"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Select Month:", choices = unique(future_data$Month)),
      selectInput("year", "Select Year:", choices = unique(future_data$Year))
    ),
    
    mainPanel(
      plotOutput("speciesPlot"),
      tableOutput("summaryTable")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    future_data %>%
      filter(Month == input$month, Year == input$year)
  })
  
  output$speciesPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Predicted_Species, fill = Predicted_Species)) +
      geom_bar() +
      theme_minimal() +
      labs(title = "Predicted Species for Selected Year & Month", x = "Species", y = "Count")
  })
  
  output$summaryTable <- renderTable({
    filtered_data() %>%
      count(Predicted_Species, sort = TRUE)
  })
}

shinyApp(ui = ui, server = server)

shiny::runApp("app.R")


