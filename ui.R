

navbarPage(
  title = "R Minor",
  position = "static-top",
  collapsible = TRUE,
  
  tabPanel(
    "Provider Dashboard",
    pickerInput(
      inputId = "providerList",
      choices = c(providerids$ProjectName),
      options = list(`live-search` = TRUE),
      width = "100%"
    ),
    verbatimTextOutput("currentHHs"),
    verbatimTextOutput("currentClients"),
    verbatimTextOutput("currentUnits")
  ), 

    navbarMenu("Prioritization",
               
             tabPanel("Prioritization List"),
             
             tabPanel("Contacts"),
             
             tabPanel("Veteran Active List")),
  
  tabPanel("Data Quality"),
  
  tabPanel("CoC Competition"),
  
  navbarMenu("Performance and Outcomes",
             
             tabPanel("Bed and Unit Utilization"),
             
             tabPanel("Recurrence"))
  )
