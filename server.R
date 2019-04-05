
function(input, output) {
  
  # produce data
  
  # theData <-  reactive({
  #   
  #   input$d_startDate <- ReportStart
  #   
  # })

  # Summary 
  
  output$summary  <-  renderText({
    
    paste0("this is the roughest of rough drafts you've ever seen.")
  })
  
  # trend
  
  output$project  <-  renderDataTable({ 
    
    Project %>% filter(ymd(OperatingEndDate) > mdy(input$d_startdate) | is.na(OperatingEndDate))

  })
  

  
}