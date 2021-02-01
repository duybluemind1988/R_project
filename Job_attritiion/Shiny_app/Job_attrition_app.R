library(data.table)
library(tidyverse)
library(shiny)
library(scales)
library(rsample) 
library(caret)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- "WA_Fn-UseC_-HR-Employee-Attrition.csv"
ui <- fluidPage(
  
  titlePanel("Job Attrition Analytics"),
  h5("Created by: DNN - Version: 1.0"),
  sidebarLayout(
    
    sidebarPanel(
      "This app was created for basic analytic job attrition"
    ), #endsidebarpanel
    
    mainPanel(
      "First some rows of data:",
      dataTableOutput("data_head_DT"),
      selectInput("cat_compare", "Choose category column to compare", choices=NULL, selected=NULL),
      plotOutput("cat_vs_cat_chart"),
      selectInput("num_compare", "Choose numeric column to compare", choices=NULL, selected=NULL),
      plotOutput("cat_vs_num_chart"),
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
  
  observeEvent(data(),updateSelectInput(session, "num_compare", 
                                        choices=names(data() %>% select_if(is.numeric)),selected='Age'))
  # Plot categorical vs categorical
  output$cat_vs_cat_chart<-renderPlot({
    #Categorical_vs_categorical_plot(data(),~Attrition,input$cat_compare)
    data() %>%
      group_by_(~Attrition, input$cat_compare) %>%
      summarize(n = n()) %>% 
      mutate(pct = n/sum(n),lbl = scales::percent(pct)) %>% 
      ggplot(aes_string(x = "Attrition",y = "pct",
                 fill = input$cat_compare)) + 
      geom_bar(stat = "identity",position = "fill") +
      scale_y_continuous(breaks = seq(0, 1, .2),label = percent) +
      geom_text(aes(label = lbl), 
                size = 3, 
                position = position_stack(vjust = 0.5)) +
      scale_fill_brewer(palette = "Set2") +
      #labs(y = "Percent",fill = "Drive Train",x = "Class",title = "Automobile Drive by Class") +
      theme_minimal() 
    
  })
  
  # Plot Categorical vs. Quantitative
  output$cat_vs_num_chart<-renderPlot({
    #Categorical_vs_quantitative_plot(data(),~Attrition,input$num_compare)
    # plot the distribution using violin and boxplots
    ggplot(data(), aes_string(x = "Attrition", 
                      y = input$num_compare)) +
      geom_violin(fill = "cornflowerblue") +
      geom_boxplot(width = .2, 
                   fill = "orange",
                   outlier.color = "orange",
                   outlier.size = 2) 
  })
}


shinyApp(ui = ui, server = server)