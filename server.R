
function(input, output, session) {
  observeEvent(c(input$providerList), {
    
    output$currentHHs <- renderInfoBox({
      infoBox(
        "Households",
        color = "aqua",
        icon = icon("users"),
        width = 1,
        Enrollment %>%
          left_join(., providerids, by = "ProjectID") %>%
          filter(is.na(ExitDate) &
                   ProjectName == input$providerList) %>%
          group_by(HouseholdID) %>%
          summarise(Total = n_distinct(HouseholdID)) %>%
          ungroup() %>%
          summarise(Households = sum(Total))
      )
    })
    
    output$currentUnits <- renderValueBox({
      valueBox(
        UnitCapacity %>%
          filter(ProjectName == input$providerList) %>%
          select(UnitCount),
        "Unit Capacity",
        color = "aqua",
        width = 1
      )
    })
    
    output$currentClients <- renderInfoBox({
      infoBox(
        "Clients",
        color = "purple",
        width = 1,
        icon = icon("user"),
        Enrollment %>%
          left_join(., providerids, by = "ProjectID") %>%
          filter(is.na(ExitDate) &
                   ProjectName == input$providerList) %>%
          group_by(PersonalID) %>%
          summarise(Total = n_distinct(PersonalID)) %>%
          ungroup() %>%
          summarise(Clients = sum(Total))
      )
    })
    
    output$currentBeds <- renderValueBox({
      valueBox(
        BedCapacity %>%
          filter(ProjectName == input$providerList) %>%
          select(BedCount),
        "Bed Capacity",
        color = "purple",
        width = 1
      )
    })
    
  })
  
  
}