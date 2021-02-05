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
      "First some rows of data:",
      dataTableOutput("data_head_DT"),
      selectInput("cat_compare", "Choose category column to compare", choices=NULL, selected=NULL),
      plotOutput("cat_vs_cat_chart"),
      plotOutput("cat_vs_cat_chart2"),
      selectInput("num_compare", "Choose numeric column to compare", choices=NULL, selected=NULL),
      plotOutput("cat_vs_num_chart"),
      plotOutput("density_chart"),
      "Variable important H2O model:",
      plotOutput("variable_important"),
      #"Table test set",
      #tableOutput("test_set"),
      "Shap summary plot via test set:",
      plotOutput("shap_summary_plot")
    )#end mainpanel
  )# end sidebarlayout
)
