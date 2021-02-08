shinyApp(
  shinyUI(
    fluidPage(
      mainPanel(
        tabsetPanel(
          tabPanel("Summary", dataTableOutput("dis")),
          tabPanel("Plot",
                   fluidRow(
                     column(8, plotOutput("plot1")),
                     column(12, plotOutput("plot2"))
                   ))
        )
      )
    )
  ),
  shinyServer(function(input, output) {
    output$plot1 <- renderPlot({
      plot(1:10, 1:10)
    })
    
    output$plot2 <- renderPlot({
      plot(1:10 ,10:1)
    })
    
    output$dis <- renderDataTable({})
  })
)
