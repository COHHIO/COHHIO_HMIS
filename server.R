
function(input, output, session) {
  observeEvent(c(input$providerList), {
    
    output$currentHHs <- renderPrint({
      Enrollment %>%
        left_join(., providerids, by = "ProjectID") %>%
        filter(is.na(ExitDate) &
                 ProjectName == input$providerList) %>%
        group_by(HouseholdID) %>%
        summarise(Total = n_distinct(HouseholdID)) %>%
        ungroup() %>%
        summarise(Households = sum(Total))
    })
    
    output$currentClients <- renderPrint({
      Enrollment %>%
        left_join(., providerids, by = "ProjectID") %>%
        filter(is.na(ExitDate) &
                 ProjectName == input$providerList) %>%
        group_by(PersonalID) %>%
        summarise(Total = n_distinct(PersonalID)) %>%
        ungroup() %>%
        summarise(Clients = sum(Total))
    })
    
    output$currentUnits <- renderPrint({
      UnitCapacity %>%
        filter(ProjectName == input$providerList) %>%
        select(UnitCount)
    })
    
  })
  
  
}