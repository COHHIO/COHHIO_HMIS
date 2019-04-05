
fluidPage(
  
  titlePanel("DRAFT"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput(
        inputId = "d_startdate",
        label = "Report Start Date",
        format = "mm/dd/yyyy",
        value = mdy("1/1/2019")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", textOutput("summary")),
        tabPanel("Project", dataTableOutput("project"))
      )
    )
  )
)
