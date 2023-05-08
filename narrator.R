library(shiny)
library(narrator)
library(dplyr)
library(tidyr)
library(bslib)
library(shinyWidgets)
library(prophet)
library(DT)

fit_prophet <- function(data) {
  model <- suppressMessages(prophet(data))
  future <- make_future_dataframe(model, periods = 12, freq = "month")
  forecast <- predict(model, future)
  return(forecast)
}

grouped_data <- sales %>%
  dplyr::mutate(ds = lubridate::floor_date(Date, unit = "month")) %>%
  dplyr::group_by(Region, ds) %>%
  dplyr::summarise(y = sum(Sales, na.rm = TRUE)) %>%
  tidyr::nest()

grouped_data$forecast <- lapply(grouped_data$data, fit_prophet)

actuals <- grouped_data %>%
  dplyr::select(-forecast) %>%
  unnest(data)

forecast <- grouped_data %>%
  dplyr::select(-data) %>%
  unnest(forecast) %>%
  dplyr::select(ds, yhat) %>%
  dplyr::left_join(actuals) %>%
  suppressMessages() %>%
  dplyr::rename(Actuals = y,
                Forecast = yhat)

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
      tabsetPanel(
        tabPanel(
          title = "Descriptive",
          narratorUI(id = "descriptive")
        ),
        tabPanel(
          title = "Trend",
          narratorUI(id = "trend")
        ),
        tabPanel(
          title = "Forecast",
          narratorUI(id = "forecast")
        )
      )
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
      id = "descriptive",
      narrative_type = "descriptive",
      df = reactive(sales),
      format = TRUE,
      summarization = input$summarization,
      measure = input$measure,
      dimensions = input$dimensions
    )
    
    narratorServer(
      id = "trend",
      narrative_type = "trend",
      df = reactive(sales),
      format = TRUE,
      summarization = input$summarization,
      measure = input$measure,
      dimensions = input$dimensions
    )
    
    narratorServer(
      id = "forecast",
      narrative_type = "forecast",
      df = reactive(forecast),
      format = TRUE,
      summarization = input$summarization,
      measure = input$measure,
      dimensions = input$dimensions
    )
  })
}

shinyApp(ui, server)
