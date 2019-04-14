
function(input, output, session) {
  observeEvent(c(input$providerList), {
    output$currentHHs <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Current Households",
            color = "aqua",
            icon = icon("users"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(Households)
          )
        })
      }
    else{
      
    }
    
    output$currentUnits <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Unit Capacity",
            color = "aqua",
            icon = icon("building"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(UnitCount)
          )
        })
      }
    else{
      
    }
    
    output$currentUnitUtilization <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Current Unit Utilization",
            color = "aqua",
            icon = icon("building"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(UnitUtilization)
          )
        })
      }
    else{
      
    }
    
    output$currentClients <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Current Clients",
            color = "purple",
            icon = icon("user"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(Clients)
          )
        })
      }
    else{
      
    }
    
    output$currentBeds <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0)
      {
        renderInfoBox({
          infoBox(
            "Bed Capacity",
            color = "purple",
            icon = icon("bed"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(BedCount)
          )
        })
      }
    else{
      
    }
    
    output$currentBedUtilization <-
      if (nrow(Utilization %>%
               filter(
                 ProjectName == input$providerList &
                 ProjectType %in% c(1, 2, 3, 8, 9)
               )) > 0) {
        renderInfoBox({
          infoBox(
            "Current Bed Utilization",
            color = "purple",
            icon = icon("bed"),
            Utilization %>%
              filter(ProjectName == input$providerList) %>%
              select(BedUtilization)
          )
        })
      }
    else{
      
    }
  })
  
  observeEvent(
    c(input$regionList),
    {
      output$SPDATScoresByCounty <-
        renderDataTable(
          Compare %>%
            filter(RegionName == input$regionList) %>%
            select(
              "County Where Served" = CountyServed,
              "Avg County Score" = AverageScore,
              "# Households in County" = HHsLHinCounty,
              "Housed Avg Score" = HousedAverageScore,
              "# Households Housed in County" = HHsHousedInCounty
            )
        )
  })
  
  
}