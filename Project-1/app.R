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
                selected = c("AL", "NC"))#,
    # Year Selection
   # sliderInput("yearSelect",
    #            "Year Erected:",
     #           min = min(monuments.load$year, na.rm = T),
      #          max = max(monuments.load$year, na.rm = T),
       #         value = c(min(monuments.load$year, na.rm = T), max(monuments.load$year, na.rm = T)),
        #        step = 1)
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
                   tabPanel("Side", plotlyOutput("plot_side")),
                   tabPanel("Year", plotlyOutput("plot_year")),
                   tabPanel("Type", plotlyOutput("plot_type")))
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
    monuments <- monuments.load %>%
      # Slider Filter
#      filter(year >= input$yearSelect[1] & year <= input$yearSelect[2])
    # Homeworld Filter
    if (length(input$stateSelect) > 0 ) {
      monuments <- subset(monuments, state %in% input$stateSelect)
    }
    return(monuments)
  })
  
  # Plot 1
  output$plot1 <- renderPlotly({
    dat <- mmInput()
    ggplotly(
      ggplot(data = dat, aes(x = state, y = number, color = "blue")) + 
        geom_point() +
        guides(color = FALSE)
      , tooltip = "text")
  })
  
  # A plot showing the side of the state
  output$plot_side <- renderPlotly({
    dat <- mmInput()
    ggplot(data = dat, aes(x = state, y = as.numeric(number), fill = state)) + geom_bar(stat = "identity")
  })
  # A plot showing the year of the monument
  output$plot_year <- renderPlotly({
    dat <- mmInput()
    ggplot(data = dat, aes(x = year, y = as.numeric(value), fill = year)) + geom_bar(stat = "identity")
  })
  # A plot showing type of monument
  output$plot_type <- renderPlotly({
    dat <- mmInput()
    ggplot(data = dat, aes(x = year, y = as.numeric(value), fill = year)) + geom_bar(stat = "identity")
  })
  # Data table of monumnets
  output$table <- DT::renderDataTable({
    subset(mmInput(), select = c(state, type, year, status))
  })
  # Per state mean info box
  output$side <- renderInfoBox({
    mm <- mmInput()
    num <- round(mean(mm$state, na.rm = T), 2)
    
    infoBox("Avg Per State", value = num, subtitle = paste(nrow(mm), "state"), icon = icon("balance-scale"), color = "purple")
  })
  # Per state mean info box
  output$side <- renderInfoBox({
    mm <- mmInput()
    num <- round(mean(mm$state, na.rm = T), 2)
    
    infoBox("Avg Mass", value = num, subtitle = paste(nrow(mm), "year"), color = "blue")
  })
  # Height mean value box
  output$side <- renderValueBox({
    mm <- mmInput()
    num <- round(mean(mm$state, na.rm = T), 2)
    
    valueBox(subtitle = "Avg Monuments", value = num, color = "grey")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)