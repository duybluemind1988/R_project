#1. library ----
# Shiny
library(shiny)
library(shinydashboard)
# Modeling
library(survival)
library(parsnip)
library(broom)
library(recipes)
library(tictoc)
library(scales)
library(caret)
# Advanced ML
library(h2o)
library(lime)
h2o.init()
h2o.no_progress() # diseable progress bar
# EDA
library(correlationfunnel)

# Core & Data Viz
library(data.table)
library(tidyverse)
library(plotly)
library(tidyquant)

#2. Load data and function ----
path<-'/home/dnn/Data_science/Git/R_project/Churn/WA_Fn-UseC_-Telco-Customer-Churn.csv'
# load the model
model_path <- "/home/dnn/Data_science/Git/R_project/Churn/Shiny/dnn_shiny/GBM_model_R_1613740880034_453"
recipe_path <- "/home/dnn/Data_science/Git/R_project/Churn/Shiny/dnn_shiny/recipe.Rds"

h2o_model_load <- h2o.loadModel(model_path)
recipe_load <- readr::read_rds(recipe_path)

server <- function(input, output, session) {

#3. EDA---- 
  data_raw <- reactive({fread(path)})
  
  data <- reactive({
    fread(path) %>% 
      drop_na() %>%
      select(-customerID)
  })
  
  output$data_head_DT<-renderDataTable(data_raw(),
                                       options =list(pageLength = 5)
  )
  # Correlation funnel
  output$correlation_funnel<-renderPlotly({
    data() %>%
      binarize() %>%
      correlate(Churn__Yes) %>% 
      plot_correlation_funnel(interactive = TRUE, alpha = 0.7)
  })
 
  
  
  observeEvent(data(),updateSelectInput(session, "category_column1", 
                                        choices=names(data() %>% select_if(is.character)),selected="Contract"))
  observeEvent(data(),updateSelectInput(session, "category_column2", 
                                        choices=names(data() %>% select_if(is.character)),selected="gender"))
  # Plot categorical vs categorical
  Categorical_vs_categorical_plot <- function(data,group_col,fill_col){
    p<-  data %>%
        group_by_(group_col, fill_col) %>%
        summarize(n = n()) %>% 
        mutate(pct = n/sum(n),lbl = scales::percent(pct))%>% 
        ggplot(aes_string(x = group_col,y = "pct",fill = fill_col)) +
        geom_bar(stat = "identity",
                 position = "fill") +
        scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
        geom_text(aes(label = lbl), 
                  size = 3, 
                  position = position_stack(vjust = 0.5)) +
        #scale_fill_brewer(palette = "Set2") +
        labs(y = "Percent",x = group_col,title = "Compare attrition accross category")+
        theme_minimal() 
    p
    #ggplotly(p)
    }
  # output$cat_vs_cat_chart1<-renderPlot({
  #   Categorical_vs_categorical_plot(data(),"Churn",input$category_column)
  # })
  
  # Plot facet  categorical
  output$cat_vs_cat_chart2<-renderPlot({
    Categorical_vs_categorical_plot(data(),input$category_column1,"Churn")
  })
  
  Categorical_facet_plot <- function(data,group_col,facet_col,target_col){
    data %>%
      group_by_(group_col, facet_col,target_col) %>%
      summarize(n = n()) %>% 
      mutate(pct = n/sum(n),lbl = scales::percent(pct))%>% 
      ggplot(aes_string(x = group_col,y = "pct",
                  fill = target_col)) +
      geom_bar(stat = "identity",
               position = "fill") +
      facet_wrap( facet_col ) + 
      scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
      geom_text(aes(label = lbl), 
                size = 3, 
                position = position_stack(vjust = 0.5)) +
      #scale_fill_brewer(palette = "Set2") +
      labs(y = "Percent",x = group_col,title = facet_col)+
      theme_minimal()  
  }
  output$Categorical_facet_plot<-renderPlot({
    Categorical_facet_plot(data(),input$category_column1,input$category_column2,"Churn")
  })
  
  observeEvent(data(),updateSelectInput(session, "numeric_column", 
                                        choices=names(data() %>% select_if(is.numeric)),selected="tenure"))
  
  ## Plot Categorical vs. Quantitative
  Categorical_vs_quantitative_plot <- function(data,categorical_col,quantitative_col){
    # plot the distribution using violin and boxplots
    p <- ggplot(data, aes_string(x = categorical_col, 
                            y = quantitative_col)) +
      geom_violin(fill = "cornflowerblue") +
      geom_boxplot(width = .2, 
                   fill = "orange",
                   outlier.color = "orange",
                   outlier.size = 2) +
      labs(title = "Box plot")
    #ggplotly(p)
    p
  }
  output$cat_vs_num_chart<-renderPlot({
    Categorical_vs_quantitative_plot(data(),"Churn",input$numeric_column)
  })
  
  ## density plot
  density_plot <-function(data,categorical_col,quantitative_col ){
    ggplot(data, 
           aes_string(x = quantitative_col, fill = categorical_col)) + 
      geom_density(alpha = 0.7) + 
      scale_fill_manual(values = c("#386cb0","#fdb462"))+
      labs(title = "Density plot")
  }
  
  output$density_chart<-renderPlot({
    density_plot(data(),"Churn",input$numeric_column)
  })
  
  # Plot combine category and quantitative:
  
  observeEvent(data(),updateSelectInput(session, "category_column3", 
                                        choices=names(data() %>% select_if(is.character)),selected="Contract"))
  
  observeEvent(data(),updateSelectInput(session, "numeric_column2", 
                                        choices=names(data() %>% select_if(is.numeric)),selected="tenure"))
  
  facet_plot <- function(data,categorical_col,quantitative_col,facet_col){
    # plot the distribution using violin and boxplots
    ggplot(data, aes_string(x = categorical_col, 
                            y = quantitative_col)) +
      geom_violin(fill = "cornflowerblue") +
      geom_boxplot(width = .2, 
                   fill = "orange",
                   outlier.color = "orange",
                   outlier.size = 2) +
      facet_wrap(facet_col) +
      labs(title=facet_col)
  }
  output$facet_chart<-renderPlot({
    facet_plot(data(),"Churn",input$numeric_column,input$category_column3)
  })
  
  # 4. Survival analysis ----
  
  plot_customer_survival <- function(object_survfit) {
    
    g <- tidy(object_survfit) %>%
      ggplot(aes(time, estimate, color = strata)) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
      geom_line(size = 1) +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = "Customer survival", color = "Contract Type", 
           x = "Days After Purchase", y = "Percentage of Customers Staying")
    
    ggplotly(g)
  }
  plot_customer_loss <- function(object_survfit) {
    
    g <- tidy(object_survfit) %>%
      mutate(customers_lost = 1 - estimate) %>%
      ggplot(aes(time, customers_lost, color = strata)) +
      geom_line(size = 1) +
      theme_tq() +
      scale_color_tq() +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = "Customer loss", color = "Contract Type", 
           x = "Days After Purchase", y = "Percentage of Customers Lost")
    
    ggplotly(g)
  }
  
  data_surv <- reactive({
    data() %>%
      mutate(
        Churn_Yes                     = Churn == "Yes",
      ) %>% 
      select(
        tenure, Churn_Yes, Contract, OnlineSecurity, TechSupport, InternetService,
        PaymentMethod, DeviceProtection
      )
  })
  # Model Kaplan-Meier ----
  model_Kaplan_Meier_func <- function(data,time,event,group){
    lhs <- paste("Surv(",time,",",event,")")
    rhs <- paste("strata(",group,")")
    form = as.formula(paste(lhs, "~", rhs))
    model <- survfit(form, data)
    model
  }
  survfit_simple_model <- reactive(model_Kaplan_Meier_func(data_surv(),"tenure","Churn_Yes",input$category_column4))
  output$KM_model_table <- renderDataTable(as.data.table(tidy(survfit_simple_model())),
                                           options =list(pageLength = 5))
                  
  output$KM_model_survfit <- renderPrint(survfit_simple_model())
  output$KM_plot_survival <-renderPlotly(
    plot_customer_survival(survfit_simple_model())
  )
  output$KM_plot_loss <-renderPlotly(
    plot_customer_loss(survfit_simple_model())
  )
  
  # Model coxph ----
  observeEvent(data(),updateSelectInput(session, "category_column4", 
                                        choices=names(data_surv() %>% select_if(is.character)),selected="Contract"))
  
  model_coxph_func <- function(data,time,event,group){
    lhs <- paste("Surv(",time,",",event,")")
    rhs <- paste("-",group,"+strata(",group,")")
    form = as.formula(paste(lhs, "~ .", rhs))
    model_coxph <- coxph(form, data)
    model_coxph
  }
  coxph_model <- reactive(model_coxph_func(data_surv(),"tenure","Churn_Yes",input$category_column4))
  
  output$coxph_model_table <- renderDataTable(as.data.table(coxph_model() %>% 
                                                              survfit() %>% tidy()),
                                           options =list(pageLength = 5))
  output$coxph_model_infor <- renderPrint(coxph_model())
  output$coxph_model_survfit <- renderPrint(coxph_model() %>% 
                                              survfit())
  output$coxph_plot_survival <-renderPlotly(
    plot_customer_survival(coxph_model() %>% 
                             survfit())
  )
  output$coxph_plot_loss <-renderPlotly(
    plot_customer_loss(coxph_model() %>% 
                             survfit())
  )
  
  #5. Model explain ----
  test_set <- reactive({
    set.seed(430)
    split = caret::createDataPartition(data()$Churn, p =0.8, list = FALSE)
    #train = data()[split, ]
    test = data()[-split, ]
    test <- bake(recipe_load, test)
    test <- as.h2o(test)
    #test <- as.data.frame(test)
    test
  })
  train_set <- reactive({
    set.seed(430)
    split = caret::createDataPartition(data()$Churn, p =0.8, list = FALSE)
    train = data()[split, ]
    train <- bake(recipe_load, train)
    train <- as.h2o(train)
    #test <- as.data.frame(test)
    train
  })
  # Variable important plot
  output$variable_important<-renderPlot({
    h2o.varimp_plot(h2o_model_load)
  })
  
  # Shap summary plot
  output$shap_summary_plot<-renderPlot({
    h2o.shap_summary_plot(h2o_model_load,test_set())
  })
  
  # Partial Dependence (PD) Plots
  
  observeEvent(data(),updateSelectInput(session, "all_column", 
                                        choices=names(data()),selected="Contract"))
  
  output$partial_dependence_plot <- renderPlot({
    h2o.pd_plot(h2o_model_load, test_set(), column = input$all_column)
  })
  # Individual Conditional Expectiation (ICE) Plots
  
  output$Individual_Conditional_Expectiation_plot  <- renderPlot({
    h2o.ice_plot(h2o_model_load, test_set(), column = input$all_column)
  })
  # H2o Model performance
  output$h2o_performance <- renderPrint({
    h2o.performance(h2o_model_load,test_set())
  })
  # Model confusion matrix
  output$confusionmatrix <- renderPrint({
    test_pred <- h2o.predict(h2o_model_load, test_set()[,-c(33)]) 
    predict <- as.data.frame(test_pred)$predict
    reference <- as.data.frame(test_set())$Churn
    matrix <- confusionMatrix(predict,reference,positive = "Yes",mode="prec_recall")
    matrix
  })
  #6. Predict test set ----
  
  # Show all test set by data.table (add ID)
  # test_set_prediction_DT the same as test set, but not bake and as h2o (contain customer ID)
  test_set_prediction_DT <- reactive({
    set.seed(430)
    data <- data_raw() %>% drop_na()
    split = caret::createDataPartition(data$Churn, p =0.8, list = FALSE)
    #train = data()[split, ]
    test = data[-split, ]
    test$Id <- seq.int(nrow(test))
    setcolorder(test, c("Id", setdiff(names(test), "Id")))
    # Khong duoc dung bake o day
    #test <- bake(recipe_load, test) 
  })
  
  output$test_set_DT<-renderDataTable({
    test_set_prediction_DT()
  },options =list(pageLength = 5))
  
  # Show all customer ID for choosen
  observeEvent(test_set_prediction_DT(),updateSelectInput(session, "custome_id",
                                        choices=test_set_prediction_DT() %>% select(customerID),selected="9237-HQITU"))
  # prepare test row for prediction:
  test_row_choose <- reactive({
    test_set_prediction_DT() %>% 
      select(-Id) %>% 
      filter(customerID == input$custome_id)
  })

  # Show test row choose all features
  output$test_row_choose_DT <- renderDataTable(test_row_choose())

  # Process test row choose
  test_row_choose_process <- reactive ({
    test <- bake(recipe_load, test_row_choose())
    test <- as.h2o(test)
  })

  # Predict test set (remove class column - no different)
  output$test_predict <- renderTable({
    test_pred <- h2o.predict(h2o_model_load, test_row_choose_process())
  })

  # Shap explain row plot: (Problem here)
  output$Shap_Explain_Row_plot  <- renderPlot({
    #rownames(test_set_prediction_DT()) <- test_set_prediction_DT() %>% select(customerID) #invalid (NULL) left side of assignment
    #row_number <- which(rownames(test_set_prediction_DT())== input$custome_id)
    row_number <- test_set_prediction_DT() %>% 
                    filter(customerID == input$custome_id) %>% 
                    pull(Id)
    h2o.shap_explain_row_plot(h2o_model_load, test_set(),row_index=row_number)
  })

  # LIME explain row plot:
  output$Lime_Explain_Row_plot  <- renderPlot({
    # Run lime() on training set
    explainer <- lime::lime(
      as.data.frame(train_set()),
      model          = h2o_model_load,
      bin_continuous = FALSE)
    # Run explain() on explainer
    explanation <- lime::explain(
      as.data.frame(test_row_choose_process()),
      explainer    = explainer,
      n_labels     = 1,
      n_features   = 10,
      kernel_width = 0.5)
    p1 <- lime::plot_features(explanation) +
      labs(title = "HR Predictive Analytics: LIME Feature Importance Visualization",
           subtitle = "Hold Out (Test) Set, First 10 Cases Shown")
    p1

  })
}