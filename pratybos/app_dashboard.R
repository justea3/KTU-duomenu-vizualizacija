library(shinydashboard)
library(tidyverse)

ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Paskaita 2"),
  dashboardSidebar(textInput("imones_kodas", "Imones kodas", value = "")),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      fluidRow(plotOutput("plot", height = 250)),
      fluidRow(tableOutput("table"))
    )
  )
)

server <- function(input, output) {
  data <- read_csv("../laboratorinis/data/lab_sodra.csv")
  
  output$table <- renderTable(
    data %>%
      filter(jarCode == input$imones_kodas) , digits = 0
  )
  
  output$plot <- renderPlot(
    data %>%
      filter(jarCode == input$imones_kodas) %>%
      ggplot(aes(x = month, y = numInsured)) +
      geom_line() +
      geom_point() +
      theme_minimal()
  )
}

shinyApp(ui, server)