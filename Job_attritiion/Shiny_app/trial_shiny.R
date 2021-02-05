library(tidyverse)
library(modeldata)
library(shiny)
data(attrition)

ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      "This app was created for basic analytic job attrition"
    ), #endsidebarpanel
    mainPanel(
      tableOutput("data"),
      plotOutput("cat_vs_cat_chart2"),
    )#end mainpanel
  )# end sidebarlayout
)

server <- function(input, output, session) {
  
  output$data <- reactive(as.data.frame(attrition))
  observeEvent(data(),updateSelectInput(session, "cat_compare", 
                                        choices=names(data() %>% select_if(is.character)),selected='BusinessTravel'))

  output$cat_vs_cat_chart2 <- renderPlot({
    data() %>%
      ggplot(aes_(x = input$cat_compare, group = ~Attrition)) + 
      geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
               stat="count", 
               alpha = 0.7) +
      geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
                stat= "count", 
                vjust = 2) +
      #labs(y = "Percentage", fill= "Education") +
      facet_grid(~Attrition) +
      theme_minimal()+
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
      ggtitle("Attrition") 
    
  })
}

shinyApp(ui = ui, server = server)