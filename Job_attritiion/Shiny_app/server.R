library(data.table)
library(tidyverse)
library(recipes)
library(shiny)
library(scales)
library(rsample) 
library(caret)
library(h2o)
h2o.init()
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- "WA_Fn-UseC_-HR-Employee-Attrition.csv"
#path <- "C:/Users/DNN/Data_science/Git/R_project/Job_attritiion/Shiny_app/WA_Fn-UseC_-HR-Employee-Attrition.csv"
# load the model
model_path <- "GBM_1_AutoML_20210204_095214"
#model_path <- "C:/Users/DNN/Data_science/Git/R_project/Job_attritiion/Shiny_app/GBM_1_AutoML_20210204_095214"
recipe_path <- "recipe.Rds"

h2o_model <- h2o.loadModel(model_path)
recipe_load <- readr::read_rds(recipe_path)

server <- function(input, output, session) {
  
  data <- reactive({fread(path)})
  
  output$data_head_DT<-renderDataTable(data(),
                                       options =list(pageLength = 5)
  )
  observeEvent(data(),updateSelectInput(session, "category_column", 
                                                choices=names(data() %>% select_if(is.character)),selected="BusinessTravel"))
  
  observeEvent(data(),updateSelectInput(session, "numeric_column", 
                                        choices=names(data() %>% select_if(is.numeric)),selected="Age"))
  
  # Plot categorical vs categorical
  output$cat_vs_cat_chart<-renderPlot({
    #Categorical_vs_categorical_plot(data(),~Attrition,input$cat_compare)
    data() %>%
      group_by_(~Attrition, input$category_column) %>%
      summarize(n = n()) %>% 
      mutate(pct = n/sum(n),lbl = scales::percent(pct)) %>% 
      ggplot(aes_string(x = "Attrition",y = "pct",
                 fill = input$category_column)) + 
      geom_bar(stat = "identity",position = "fill") +
      scale_y_continuous(breaks = seq(0, 1, .2),label = percent) +
      geom_text(aes(label = lbl), 
                size = 3, 
                position = position_stack(vjust = 0.5)) +
      scale_fill_brewer(palette = "Set2") +
      #labs(y = "Percent",fill = "Drive Train",x = "Class",title = "Automobile Drive by Class") +
      theme_minimal() 
  # Plot categorical vs categorical 2 (Problem)  
  })
  output$cat_vs_cat_chart2 <- renderPlot({
    data() %>%
      ggplot(aes_string(x = input$category_column, group = "Attrition")) + 
      #ggplot(aes_(x = ~BusinessTravel, group = ~Attrition)) + # OK
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
  # Plot Categorical vs. Quantitative
  output$cat_vs_num_chart<-renderPlot({
    #Categorical_vs_quantitative_plot(data(),~Attrition,input$num_compare)
    # plot the distribution using violin and boxplots
    ggplot(data(), aes_string(x = "Attrition", 
                      y = input$numeric_column)) +
      geom_violin(fill = "cornflowerblue") +
      geom_boxplot(width = .2, 
                   fill = "orange",
                   outlier.color = "orange",
                   outlier.size = 2) 
  })

  # Plot density chart (Problem)
  output$density_chart<-renderPlot({
    ggplot(data(),aes_string(x =input$numeric_column, fill = "Attrition")) + 
    #ggplot(data(),aes_(x =~MonthlyIncome, fill = ~Attrition)) + # OK
                  geom_density(alpha = 0.7) + 
                  scale_fill_manual(values = c("#386cb0","#fdb462"))
  })
  # Plot combine category and quantitative:
  
  observeEvent(data(),updateSelectInput(session, "category_column2", 
                                        choices=names(data() %>% select_if(is.character)),selected="Department"))
  
  observeEvent(data(),updateSelectInput(session, "numeric_column2", 
                                        choices=names(data() %>% select_if(is.numeric)),selected="MonthlyIncome"))
  
  output$complex_chart<-renderPlot({
    # plot the distribution using violin and boxplots
    ggplot(data(), aes_string(x = "Attrition", 
                               y = input$numeric_column2)) +
      geom_violin(fill = "cornflowerblue") +
      geom_boxplot(width = .2, 
                   fill = "orange",
                   outlier.color = "orange",
                   outlier.size = 2) +
      facet_wrap(input$category_column2) +
      labs(title=input$category_column2)
  })
  
  # Variable important
  output$variable_important<-renderPlot({
    h2o.varimp_plot(h2o_model)
  })
  
  test_set <- reactive({
    set.seed(430)
    split = caret::createDataPartition(data()$Attrition, p =0.8, list = FALSE)
    train = data()[split, ]
    test = data()[-split, ]
    test <- bake(recipe_load, test)
    test <- as.h2o(test)
    #test <- as.data.frame(test)
    test
  })
  #output$table_test <- renderTable(head(test_set()))
    
  # Shap summary plot
  output$shap_summary_plot<-renderPlot({
    h2o.shap_summary_plot(h2o_model,test_set())
  })
  
  # Partial Dependence (PD) Plots
  
  observeEvent(data(),updateSelectInput(session, "all_column", 
                                        choices=names(data()),selected="Department"))
  
  output$partial_dependence_plot <- renderPlot({
    h2o.pd_plot(h2o_model, test_set(), column = input$all_column)
  })
  # Individual Conditional Expectiation (ICE) Plots
  
  output$Individual_Conditional_Expectiation_plot  <- renderPlot({
    h2o.ice_plot(h2o_model, test_set(), column = input$all_column)
  })
  
  # Shap explain row plot:
  output$test_row_choose <- renderTable(test_set()[input$obs,])
  
  output$Shap_Explain_Row_plot  <- renderPlot({
    h2o.shap_explain_row_plot(h2o_model, test_set(),row_index = input$obs)
  })
  
  # Model performance
  output$performance <- renderTable(h2o.performance(h2o_model,test_set())) 
  
}

#shinyApp(ui = ui, server = server)