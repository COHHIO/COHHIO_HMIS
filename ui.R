dashboardPage(
  skin = "black",
  dashboardHeader(title = "R Minor"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenuid",
      menuItem("Provider Dashboard",
               tabName = "providerDashboardTab"),
      menuItem(
        "Prioritization",
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
      menuItem(
        "Performance and Outcomes",
        menuSubItem("Bed and Unit Utilization",
                    tabName = "utilizationTab"),
        menuSubItem("Recurrence",
                    tabName = "recurrenceTab")
      )
    ),
    HTML(paste0(
      "<br>&emsp;Last update:&emsp;",
      format(updatedate, "%m-%d-%Y %I:%M %p", tz = "US/Eastern")#,
      #      "<br>&emsp;Happy Passover and Easter and Spring Equinox!"
    ))
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "providerDashboardTab",
        pickerInput(
          inputId = "providerList",
          choices = c(providerids$ProjectName),
          options = list(`live-search` = TRUE),
          width = "100%"
        ),
        fluidRow(
          box(infoBoxOutput("currentHHs"),
              infoBoxOutput("currentUnits"),
              infoBoxOutput("currentUnitUtilization"),
              title = "Current Unit Utilization"),
          box(infoBoxOutput("currentClients"),
              infoBoxOutput("currentBeds"),
              infoBoxOutput("currentBedUtilization"),
              title = "Current Bed Utilization")
        )
      ),
      tabItem(tabName = "prioritizationListTab"),
      tabItem(tabName = "contactTab"),
      tabItem(tabName = "vetActiveListTab"),
      tabItem(tabName = "dqTab"),
      tabItem(tabName = "cocCompetitionTab"),
      tabItem(tabName = "utilizationTab"),
      tabItem(tabName = "recurrenceTab")
    )
  )
)
