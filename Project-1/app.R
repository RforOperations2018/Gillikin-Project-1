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
monuments.load <- read.csv("monuments1.csv")

header <- dashboardHeader(title = "153 Years Late: Confederate Monuments in the U.S."
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
      radioButtons("statusInput", 
                 label = h3("Status"),
                 choices = list("Active" = 1, "Removed" = 2), 
                 selected = 1),
                 fluidRow(column(3, verbatimTextOutput("value"))),
      selectInput("stateInput",
                 label = h3("Select state(s)"),
                 choices = sort(unique(monuments.load$state)),
                 multiple = TRUE),
      sliderInput("yearInput",
                label = h3("Years since Civil War"),
                min = 0,
                max = 153,
                value = c(0,153),
                step = 1)
  )
)

body <- dashboardBody(tabItems(
  tabItem("plot",
          fluidRow(
            # Count valuebox
            valueBoxOutput("count"),
            # States valuebox
            valueBoxOutput("states"),
            # Percent valuebox
            valueBoxOutput("percent")
          ),
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   # Year monument erected plot
                   tabPanel("Year Erected", plotlyOutput("plot_state")),
                   # Side state was on in the Civil War plot
                   tabPanel("Side in Civil War", plotlyOutput("plot_side")),
                   # Type of monument plot
                   tabPanel("Type of Monument", plotlyOutput("plot_type")))
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
    # Slider Year Filter
    #filter(2018 - year >= input$yearInput[1] & 2018 - year <= input$yearInput[2])
    # Status Filter
    if (input$statusInput == "Active" ) {
      monuments <- subset(monuments, year.dedicated %in% input$statusInput)
    } else {
      monuments <- subset(monuments, year.removed %in% input$statusInput)
    }
    # State Filter
    if (length(input$stateInput) > 0 ) {
      monuments <- subset(monuments, state %in% input$stateInput)
    }
    return(monuments)
  })
  # Monuments by year and total by state
  output$plot_state <- renderPlotly({
    dat <- mmInput()
    ggplotly(
      ggplot(data = dat, aes(x = year.dedicated, fill = state)) + 
        geom_bar()
    ) 
  })
  # Monuments by side
  output$plot_side <- renderPlotly({
    dat <- mmInput()
    ggplotly(
      ggplot(data = dat, aes(x = year.dedicated, fill = side)) + 
        geom_bar()
    )
  })
  # Monuments by type
  output$plot_type <- renderPlotly({
    dat <- mmInput()
    ggplotly(
      ggplot(data = dat, aes(x = state, fill = type)) + 
        geom_bar(position = "stack")
    )
  })
  # Data table of monuments
  output$table <- DT::renderDataTable({
    subset(mmInput(), select = c(state, side, type, year.dedicated, number))
  })
  # Count value box
  output$count <- renderValueBox({
    mm <- mmInput()
    num <- round(mean(mm$state, na.rm = T), 2)
    valueBox("Confederate Monuments", value = paste0(nrow(mm)), color = "blue")
  })
  # Number of states value box
  output$states <- renderValueBox({
    mm <- mmInput()
    num <- count(mm, state)
    valueBox("Number of States", value = paste0(num), color = "olive")
  })
  # Percent value box
  output$percent <- renderValueBox({
    mm <- mmInput()
    num <- round(nrow(mm)/nrow(monuments.load), 4) * 100 
    valueBox("of total", value = paste0(num,"%"), color = "teal")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)