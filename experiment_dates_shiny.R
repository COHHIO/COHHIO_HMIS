library(shiny)
library(tidyverse)
library(lubridate)

if (interactive()) {
  ui <- fluidPage(
    sliderInput("n", "Quarter", 1, 4, 
                if_else(quarter(today()) - 1 == 0, 1,
                        quarter(today()) - 1)),
    sliderInput("y", "Year", year(today()) - 2, 
                year(today()), 
                year(today()),
                sep = ""),
    dateRangeInput("inDateRange", "Input date range")
  )
  
  server <- function(input, output, session) {
    observe({
      enddate <- mdy(paste(
        case_when(
          input$n == 1 ~ "03/31/",
          input$n == 2 ~ "06/30/",
          input$n == 3 ~ "09/30/",
          input$n == 4 ~ "12/31/"
        ),
        input$y
      ))
      
      updateDateRangeInput(
        session,
        "inDateRange",
        label = paste("Year", input$y,"Q", input$n),
        start = mdy(paste("01/01/", input$y)),
        end = enddate
      )
    })
  }
  
  shinyApp(ui, server)
}