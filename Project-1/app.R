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
                selected = c("AL", "NC")),
    # Birth Selection
    sliderInput("yearSelect",
                "Year Erected:",
                min = min(monuments.load$year, na.rm = T),
                max = max(monuments.load$year, na.rm = T),
                value = c(min(monuments.load$year, na.rm = T), max(monuments.load$year, na.rm = T)),
                step = 1)
  )
)

body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            infoBoxOutput("mass"),
            valueBoxOutput("height")
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
            box(title = "Selected Character Stats", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  mmInput <- reactive({
    monuments <- monuments.load %>%
      # Slider Filter
      filter(birth_year >= input$birthSelect[1] & birth_year <= input$birthSelect[2])
    # Homeworld Filter
    if (length(input$worldSelect) > 0 ) {
      monuments <- subset(monuments, homeworld %in% input$worldSelect)
    }
    
    return(monuments)
  })
  # Reactive melted data
  moInput <- reactive({
    mmInput() %>%
      melt(id = "name")
  })
  # A plot showing the side of the state
  output$plot_side <- renderPlotly({
    dat <- subset(moInput(), variable == "side")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  # A plot showing the year of the monument
  output$plot_year <- renderPlotly({
    dat <- subset(moInput(),  variable == "year")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  # A plot showing type of monument
  output$plot_type <- renderPlotly({
    dat <- subset(moInput(),  variable == "type")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  # Data table of characters
  output$table <- DT::renderDataTable({
    subset(mmInput(), select = c(name, height, mass, birth_year, homeworld, species, films))
  })
  # Mass mean info box
  output$side <- renderInfoBox({
    mm <- mmInput()
    num <- round(mean(mm$mass, na.rm = T), 2)
    
    infoBox("Avg Mass", value = num, subtitle = paste(nrow(mm), "characters"), icon = icon("balance-scale"), color = "purple")
  })
  # Mass mean info box
  output$side <- renderInfoBox({
    mm <- mmInput()
    num <- round(mean(mm$mass, na.rm = T), 2)
    
    infoBox("Avg Mass", value = num, subtitle = paste(nrow(mm), "characters"), icon = icon("balance-scale"), color = "purple")
  })
  # Height mean value box
  output$side <- renderValueBox({
    mm <- mmInput()
    num <- round(mean(mm$height, na.rm = T), 2)
    
    valueBox(subtitle = "Avg Monuments", value = num, color = "grey")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)