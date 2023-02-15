library(shiny)
library(narrator)
library(dplyr)
library(bslib)
library(shinyWidgets)

dimensions <- sales %>%
  select(-`Order ID`, -Promotion) %>%
  select(where(is.character), where(is.factor)) %>%
  names()

measures <- sales %>%
  select(-`Order ID`, -Promotion) %>%
  select_if(is.numeric) %>%
  names()

light <- bs_theme(version = 5, bootswatch = "litera")
dark <- bs_theme(version = 5, bootswatch = "darkly")

ui <- fluidPage(
  
  theme = light, 
  
  fluidRow(
    h2("narrator")
  ),
  
  fluidRow(
    column(
      width = 2,
      selectizeInput(
        inputId = "measure",
        label = "Measure",
        choices = measures,
        selected = measures[3]
      ),
      selectizeInput(
        inputId = "dimensions",
        label = "Dimensions",
        choices = dimensions,
        selected = dimensions,
        multiple = TRUE
      ),
      selectizeInput(
        inputId = "summarization",
        label = "Summarization",
        choices = c("sum", "count", "average"),
        selected = "sum"
      ),
      materialSwitch(
        inputId = "dark_mode", 
        label = "Dark mode"
      ),
      tags$a("package website", href = "https://denisabd.github.io/narrator/index.html")
    ),
    column(
      width = 10,
      narratorUI(id = "main")
    )
  )
)

server <- function(input, output, session) {
  
  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) dark else light
  ))
  
  observeEvent(c(input$measure, input$dimensions, input$summarization),{
    req(input$measure, input$dimensions, input$summarization)
    
    narratorServer(
      id = "main",
      df = reactive(sales),
      format = TRUE,
      summarization = input$summarization,
      measure = input$measure,
      dimensions = input$dimensions
    )
  })
}

shinyApp(ui, server)
