

function(input, output, session) {
  observeEvent(c(input$providerList), {
    output$currentHHs <- renderInfoBox({
      infoBox(
        "Households",
        color = "aqua",
        icon = icon("users"),
        Utilization %>%
          filter(ProjectName == input$providerList) %>%
          select(Households)
      )
    })
    
    output$currentUnits <- renderInfoBox({
      infoBox(
        "Unit Capacity",
        color = "aqua",
        icon = icon("building"),
        Utilization %>%
          filter(ProjectName == input$providerList) %>%
          select(UnitCount)
      )
    })
    
    output$currentUnitUtilization <- renderInfoBox({
      infoBox(
        "Unit Utilization",
        color = "aqua",
        icon = icon("building"),
        Utilization %>%
          filter(ProjectName == input$providerList) %>%
          select(UnitUtilization)
      )
    })
    
    output$currentClients <- renderInfoBox({
      infoBox(
        "Clients",
        color = "purple",
        icon = icon("user"),
        Utilization %>%
          filter(ProjectName == input$providerList) %>%
          select(Clients)
      )
    })
    
    output$currentBeds <- renderInfoBox({
      infoBox(
        "Bed Capacity",
        color = "purple",
        icon = icon("bed"),
        Utilization %>%
          filter(ProjectName == input$providerList) %>%
          select(BedCount)
      )
    })

    output$currentBedUtilization <- renderInfoBox({
      infoBox(
        "Bed Utilization",
        color = "purple",
        icon = icon("bed"),
        Utilization %>%
          filter(ProjectName == input$providerList) %>%
          select(BedUtilization)
      )
    })    
  })
  
  
}