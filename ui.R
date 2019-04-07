

navbarPage(
  title = "R Minor",
  position = "static-top",
  collapsible = TRUE,
  
  tabPanel(
    "Provider Dashboard",
    pickerInput(
      inputId = "providerList",
      choices = c(providerids$ProjectName),
      options = list(`live-search` = TRUE)
    ),
    verbatimTextOutput("currentHHs"),
    verbatimTextOutput("currentClients"),
    verbatimTextOutput("currentUnits")
  ), 
  navbarMenu("Prioritization",
             tabPanel("Prioritization List"),
             tabPanel("Contacts"),
             tabPanel("For Download")),
  tabPanel("Data Quality")
  )
