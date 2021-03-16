library(shiny)
library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(viridis)
library(ggplot2)

ui <- fluidPage(
    titlePanel("412000 Gyvenamuju ir negyvenamuju namu statyba"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "imones_pavadinimas", label = "Imones pavadinimas", choices = NULL, selected = NULL)
        ),
        mainPanel(tabsetPanel(
            tabPanel("Darbuotoju grafikas", plotOutput("grafikas1")),
            tabPanel("Lentele", tableOutput("table")),
            tabPanel("Atlyginimu grafikas", plotOutput("grafikas2"))
        )
        )
    )
)
server <- function(input, output, session) {
    data <- read_csv("../data/lab_sodra.csv")
    dataByCode <- data %>% filter(ecoActCode == "412000")
    
    updateSelectizeInput(session, "imones_pavadinimas", choices = dataByCode$name, server = TRUE)
    
    output$table <- renderTable(
        dataByCode %>%
            filter(name == input$imones_pavadinimas) , digits = 0
    )
    
    output$grafikas1 <- renderPlot(
        dataByCode %>%
            filter(name == input$imones_pavadinimas) %>%
            ggplot(aes(x = month, y = numInsured)) +
            geom_line(col = "black") + geom_point()      
    )
    
    output$grafikas2 <- renderPlot(
        dataByCode %>%
            filter(name == input$imones_pavadinimas) %>%
            ggplot(aes(x = month, y = avgWage)) +
            geom_line(col = "black") + geom_point()      
    )
}
shinyApp(ui, server)