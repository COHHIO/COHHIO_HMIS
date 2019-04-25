

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
  output$ReportStart  <- renderText({
    ymd(input$inDateRange[1])
  })
  output$ReportEnd  <- renderText({
    ymd(input$inDateRange[2])
  })
  observeEvent(c(input$regionList, 
                 input$inDateRange,
                 input$y,
                 input$q), {
    updateDateRangeInput(
      session,
      "inDateRange",
      label = "",
      start = mdy(paste("01/01/", input$y)),
      end = mdy(paste(
        case_when(
          input$q == 1 ~ "03/31/",
          input$q == 2 ~ "06/30/",
          input$q == 3 ~ "09/30/",
          input$q == 4 ~ "12/31/"
        ),
        input$y
      ))
    )
  })    
  
  reactiveA <- reactive(
    ClientScoresInCounty <- CountyData %>%
        filter(
          served_between(CountyData, 
                         ymd(input$inDateRange[1]), 
                         ymd(input$inDateRange[2]))
        ) %>%
        select(CountyServed, PersonalID, Score) %>%
        distinct()
  )
  
  reactiveB <- reactive(
    CountyAverageScores <- ClientScoresInCounty %>%
        group_by(CountyServed) %>%
        summarise(
          AverageScore = round(mean(Score), 1),
          HHsLHinCounty = n()
        )
  )
  
  reactiveC <- reactive(
    ProviderAverages <- SPDATsByProject %>%
        filter(
          served_between(SPDATsByProject, 
                         ymd(input$inDateRange[1]), 
                         ymd(input$inDateRange[2]))
        ) %>%
        select(EnrollmentID, ProjectName, ScoreAdjusted) %>%
        group_by(ProjectName) %>%
        summarise(
          AverageScore = round(mean(ScoreAdjusted), 1),
          EnrollmentCount = n()
        )
  )
  
  reactiveD <- reactive(quote({
     CountyHousedAverageScores <- SPDATsByProject %>%
        filter(
          served_between(SPDATsByProject, 
                         ymd(input$inDateRange[1]), 
                         ymd(input$inDateRange[2]))
        ) %>%
        group_by(CountyServed) %>%
        summarise(
          HousedAverageScore = round(mean(ScoreAdjusted), 1),
          HHsHousedInCounty = n())
  }), quoted = TRUE)
  
  reactiveE <- reactive(
    Compare <-
        full_join(CountyAverageScores,
                  CountyHousedAverageScores,
                  by = "CountyServed") %>%
        arrange(CountyServed) %>%
        left_join(., Regions, by = c("CountyServed" = "County")) %>% 
      filter(RegionName == input$regionList)
  )
  
  output$SPDATScoresByCounty <- reactive(
    renderPlot(
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
    ))
  
  output$qprDateRange <- renderText({
    paste(ymd(input$inDateRange[1]), "to", ymd(input$inDateRange[2]))
  })
    
  output$CountyScoresText <-
    renderText(hhsServedInCounty)
  
  output$HHsServedScoresText <-
    renderText(hhsHousedInCounty)
  
  output$NoteToUsers <-
    renderText(noteToUsers)
}