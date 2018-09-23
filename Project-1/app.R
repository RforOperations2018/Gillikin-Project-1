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
                label = h3("Select state(s)"),
                choices = sort(unique(monuments.load$state)),
                multiple = TRUE),
    checkboxGroupInput("sideInput", 
                label = h3("Select side(s)"), 
                choices = list("North" = 1, "South" = 2, "Border State" = 3, "Not a State" = 4),
                selected = FALSE)
  )
)

body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            infoBoxOutput("state")#,
            #valueBoxOutput("year")
          ),
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Year Erected", plotlyOutput("plot_state")),
                   tabPanel("Side", plotlyOutput("plot_side")))
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
    if (length(input$sideInput) > 0 ) {
      monuments <- subset(monuments, side %in% input$sideInput)
    }
    return(monuments)
  })
  # Monuments by year and total by state
  output$plot_state <- renderPlotly({
    dat <- mmInput()
    ggplotly(
      ggplot(data = dat, aes(x = year, fill = state)) + 
        geom_bar()
      )
  })
  # Monuments by year and total by state
  output$plot_side <- renderPlotly({
    dat <- mmInput()
    ggplotly(
      ggplot(data = dat, aes(x = year, fill = side)) + 
        geom_bar()
    )
  })
  # Data table of monuments
  output$table <- DT::renderDataTable({
    subset(mmInput(), select = c(state, side, type, year, number))
  })
}
# State mean info box
#output$number <- renderInfoBox({
#  mm <- mmInput()
#  num <- round(mean(mm$number, na.rm = T), 2)
#  infoBox("Avg number", value = num, subtitle = paste(nrow(mm, "state")), color = "grey")
#})

# Run the application 
shinyApp(ui = ui, server = server)