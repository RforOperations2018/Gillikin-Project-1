# Project 1
# Shiny Dashboard

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)


#load data downloaded from Southern Poverty Law Center
#https://docs.google.com/spreadsheets/d/17ps4aqRyaIfpu7KdGsy2HRZaaQiXUfLrpUbaR9yS51E/edit?usp=sharing
monuments.load <- read.csv("monuments.csv")

header <- dashboardHeader(title = "153 Years Later: Civil War Monuments in the U.S."
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    selectInput("stateInput",
                "State:",
                choices = sort(unique(monuments.load$state)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("AL", "NC"))
  )
)

body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            infoBoxOutput("state"),
            valueBoxOutput("year")
          ),
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Side", plotlyOutput("plot_state")))
          )
  ),
  tabItem("table",
          fluidPage(
            box(title = "Selected Monument Statistics", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  mmInput <- reactive({
    monuments <- monuments.load
    if (length(input$stateInput) > 0 ) {
      monuments <- subset(monuments, state %in% input$stateInput)
    }
    return(monuments)
  })
  
  # Plot 1
  output$plot_state <- renderPlotly({
    dat <- mmInput()
    ggplotly(
      ggplot(data = dat, aes(x = state, fill = type)) + 
        geom_bar()
      )
  })
  # Data table of monuments
  output$table <- DT::renderDataTable({
    subset(mmInput(), select = c(state, year, number))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)