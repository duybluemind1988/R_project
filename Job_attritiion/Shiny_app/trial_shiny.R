library(tidyverse)
library(data.table)
#library(modeldata)
library(shiny)

path <- "C:/Users/DNN/Data_science/Git/R_project/Job_attritiion/Shiny_app/WA_Fn-UseC_-HR-Employee-Attrition.csv"

ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      "This app was created for basic analytic job attrition"
    ), #endsidebarpanel
    mainPanel(
      dataTableOutput("data_head_DT"),
      
      observeEvent(data(),updateSelectInput(session, "cat_compare", 
                                           choices=names(data() %>% select_if(is.character)),selected='BusinessTravel')),
     # plotOutput("cat_vs_cat_chart2"),
    )#end mainpanel
  )# end sidebarlayout
)

server <- function(input, output, session) {
  
  data <- reactive({fread(path)})
  output$data_head_DT<-renderDataTable(data(),
                                       options =list(pageLength = 5)
  )
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

