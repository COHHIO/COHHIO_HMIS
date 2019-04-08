

dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "R Minor"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenuid",
      menuItem(
        "Provider Dashboard",
        tabName = "providerDashboardTab"),
      menuItem("Prioritization",
        menuSubItem("Prioritization List",
                    tabName = "prioritizationListTab"),
        menuSubItem("Contact",
                    tabName = "contactTab"),
        menuSubItem("Veteran Active List",
                    tabName = "vetActiveListTab")
      ),
      menuItem("Data Quality",
               tabName = "dqTab"),
      menuItem("CoC Competition",
               tabName = "cocCompetitionTab"),
      menuItem("Performance and Outcomes",
        menuSubItem("Bed and Unit Utilization",
                    tabName = "utilizationTab"),
        menuSubItem("Recurrence",
                    tabName = "recurrenceTab")
      )
    ),
    HTML(paste0(
      "<br>&emsp;Last update:&emsp;",
      format(updatedate, "%m-%d-%Y %I:%M %p", tz = "US/Eastern")#,
      #      "<br>&emsp;Happy Valentine's Day!"
    ))),
  dashboardBody(tabItems(
    tabItem(
        tabName = "providerDashboardTab",
      fluidRow(
      column(
        width = 3,
      pickerInput(
        inputId = "providerList",
        choices = c(providerids$ProjectName),
        options = list(`live-search` = TRUE),
        width = "100%"
      ),
      box(
        title = "Current",
        status = "primary",
        solidHeader = TRUE,
        infoBoxOutput("currentHHs"),
        valueBoxOutput("currentUnits"),
        infoBoxOutput("currentClients"),
        valueBoxOutput("currentBeds")
      )))
    ), 
    tabItem(
      tabName = "prioritizationListTab"
    ),
    tabItem(
      tabName = "contactTab"
    ),
    tabItem(
      tabName = "vetActiveListTab"
    ),
    tabItem(
      tabName = "dqTab"
    ),
    tabItem(
      tabName = "cocCompetitionTab"
    ),
    tabItem(
      tabName = "utilizationTab"
    ),
    tabItem(
      tabName = "recurrenceTab"
    )
  )
  )

  # tabPanel(
  #   "Provider Dashboard",
  #   pickerInput(
  #     inputId = "providerList",
  #     choices = c(providerids$ProjectName),
  #     options = list(`live-search` = TRUE),
  #     width = "100%"
  #   ),
  #   infoBoxOutput("currentHHs"),
  #   infoBoxOutput("currentClients"),
  #   valueBoxOutput("currentUnits")
  # ), 
  # 
  #   navbarMenu("Prioritization",
  #              
  #            tabPanel("Prioritization List"),
  #            
  #            tabPanel("Contacts"),
  #            
  #            tabPanel("Veteran Active List")),
  # 
  # tabPanel("Data Quality"),
  # 
  # tabPanel("CoC Competition"),
  # 
  # navbarMenu("Performance and Outcomes",
  #            
  #            tabPanel("Bed and Unit Utilization"),
  #            
  #            tabPanel("Recurrence"))
  )
