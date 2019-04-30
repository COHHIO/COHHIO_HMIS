

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

  observeEvent(c(input$y, input$q), {
    updateDateRangeInput(
      session,
      "inDateRange",
      label = "",
      start = mdy(paste0("01-01-", input$y)),
      end = mdy(paste0(
        case_when(
          input$q == 1 ~ "03-31-",
          input$q == 2 ~ "06-30",
          input$q == 3 ~ "09-30-",
          input$q == 4 ~ "12-31-"
        ),
        input$y
      ))
    )
  })    
  
  output$SPDATScoresByCounty <- 
    renderPlot({
      CountyAverageScores <- CountyData %>%
        filter(served_between(CountyData, 
                              format.Date(input$inDateRange[1], "%m-%d-%Y"), 
                              format.Date(input$inDateRange[2], "%m-%d-%Y"))) %>%
        select(CountyServed, PersonalID, Score) %>%
        distinct() %>%
        group_by(CountyServed) %>%
        summarise(AverageScore = round(mean(Score), 1),
                  HHsLHinCounty = n())
      
      CountyHousedAverageScores <- SPDATsByProject %>%
        filter(served_between(SPDATsByProject, 
                              format.Date(input$inDateRange[1], "%m-%d-%Y"), 
                              format.Date(input$inDateRange[2], "%m-%d-%Y"))) %>%
        group_by(CountyServed) %>%
        summarise(HousedAverageScore = round(mean(ScoreAdjusted), 1),
                  HHsHousedInCounty = n())
      
      Compare <-
        full_join(CountyAverageScores,
                  CountyHousedAverageScores,
                  by = "CountyServed") %>%
        arrange(CountyServed) %>%
        left_join(., Regions, by = c("CountyServed" = "County")) %>%
        filter(RegionName == input$regionList)
      
      ggplot(
        Compare,
        aes(x = CountyServed, y = AverageScore)
      ) +
        geom_point(
          aes(x = CountyServed, y = AverageScore),
          size = 10,
          shape = 95
        ) +
        theme(axis.text.x = element_text(size = 10)) +
        geom_point(
          aes(x = CountyServed, y = HousedAverageScore),
          size = 4,
          shape = 17
        ) +
        xlab(input$regionList)
    })
  
  output$qprDateRange <- renderText({
    paste(format.Date(input$inDateRange[1], "%m-%d-%Y"), "start",
          format.Date(input$inDateRange[2], "%m-%d-%Y"), "end",
          input$y, "year", 
          input$q, "quarter", 
          input$regionList, "region")
  })
    
  output$CountyScoresText <-
    renderText(hhsServedInCounty)
  
  output$HHsServedScoresText <-
    renderText(hhsHousedInCounty)
  
  output$NoteToUsers <-
    renderText(noteToUsers)
}