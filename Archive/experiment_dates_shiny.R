# COHHIO_HMIS
# Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

library(shiny)
library(tidyverse)
library(lubridate)

if (interactive()) {
  ui <- fluidPage(
    dateRangeInput("inDateRange", "Input date range"),
    sliderInput(
      "y",
      "",
      year(today()) - 2,
      year(today()),
      year(today()),
      sep = "",
      ticks = FALSE
    ),
    sliderInput(
      "q",
      "",
      1,
      4,
      if_else(quarter(today()) - 1 == 0, 1,
              quarter(today()) - 1),
      ticks = FALSE,
      pre = "Q"
    ),
    verbatimTextOutput("ReportStart"),
    verbatimTextOutput("ReportEnd")
    )
  
  server <- function(input, output, session) {
    output$ReportStart  <- renderText({
      format(input$inDateRange[1])
    })
    output$ReportEnd  <- renderText({
      format(input$inDateRange[2])
    })
    observe({
      enddate <- mdy(paste(
        case_when(
          input$q == 1 ~ "03/31/",
          input$q == 2 ~ "06/30/",
          input$q == 3 ~ "09/30/",
          input$q == 4 ~ "12/31/"
        ),
        input$y
      ))
      
      updateDateRangeInput(
        session,
        "inDateRange",
        label = paste("Year", input$y, "Q", input$q),
        start = mdy(paste("01/01/", input$y)),
        end = enddate
      )
    })
    
  }
  
  shinyApp(ui, server)
}