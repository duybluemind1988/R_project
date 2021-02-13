#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#path <- "WA_Fn-UseC_-HR-Employee-Attrition.csv"
# load the model
#model_path <- "GBM_1_AutoML_20210204_095214"
#h2o_model <- h2o.loadModel(model_path)
#recipe_load <- readr::read_rds("recipe.Rds")

ui <- fluidPage(
  
  titlePanel("Job Attrition Analytics"),
  h5("Created by: DNN - Version: 1.0"),
  sidebarLayout(
    
    sidebarPanel(
      "This app was created for basic analytic job attrition"
    ), #endsidebarpanel
    
    mainPanel(
      ### EDA
      "First some rows of data:",
      dataTableOutput("data_head_DT"),
      selectInput("category_column", "Choose category column to compare", choices=NULL, selected=NULL),
      plotOutput("cat_vs_cat_chart"),
      plotOutput("cat_vs_cat_chart2"),
      selectInput("numeric_column", "Choose numeric column to compare", choices=NULL, selected=NULL),
      plotOutput("cat_vs_num_chart"),
      plotOutput("density_chart"),
      "For complex feature compare (facet_wrap)",
      selectInput("category_column2", "Choose column for facet_wrap", choices=NULL, selected=NULL),
      selectInput("numeric_column2", "Choose numeric column to compare", choices=NULL, selected=NULL),
      plotOutput("complex_chart"),
      
      ### MODEL INFORMATION
      "Variable important H2O model:",
      plotOutput("variable_important"),
      "Shap summary plot via test set:",
      plotOutput("shap_summary_plot"),
      "Partial Dependence (PD) Plots:",
      selectInput("all_column", "Choose column for PDP and ICE plot explaination", choices=NULL, selected=NULL),
      plotOutput("partial_dependence_plot"),
      "Individual Conditional Expectiation (ICE) Plots:",
      plotOutput("Individual_Conditional_Expectiation_plot"),
      "H2O Performance",
      verbatimTextOutput("h2o_performance"),
      "Model confusion matrix:",
      verbatimTextOutput("confusionmatrix"),
      
      ### PREDICTION
      "Show all test set:",
      dataTableOutput("test_set_DT"),
      "Predict test set",
      numericInput("obs", "Please choose row on test set for prediction",value = 10,min = 1, max = 100),
      "Show test row choose",
      tableOutput("test_row_choose"), # Show test set choose all column
      "Prediction row choose",
      tableOutput("test_predict"),
      "Explain each row on test set by shap h2o: ",
      plotOutput("Shap_Explain_Row_plot"),
      "Explain each row on test set by LIME h2o: ",
      plotOutput("Lime_Explain_Row_plot"),
      # Predict base on file input:
      fileInput("file",h3("Choose csv file"),accept = ".csv"),
      "Show test row choose",
      tableOutput("test_row_choose_upload"), # Show test set choose all column
      "Prediction row choose",
      tableOutput("test_predict_upload"),
    )#end mainpanel
  )# end sidebarlayout
)
