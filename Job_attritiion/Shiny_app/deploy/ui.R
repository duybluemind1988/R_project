#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#path <- "WA_Fn-UseC_-HR-Employee-Attrition.csv"
# load the model
#model_path <- "GBM_1_AutoML_20210204_095214"
#h2o_model <- h2o.loadModel(model_path)
#recipe_load <- readr::read_rds("recipe.Rds")

ui <- fluidPage(
  
  navbarPage("Job attrition prediction",
      ### EDA ----
      tabPanel("Explore data",
          "First some rows of data:",
          dataTableOutput("data_head_DT"),
          selectInput("category_column", "Choose category column to compare", choices=NULL, selected=NULL),
          #plotOutput("cat_vs_cat_chart"),
          #plotOutput("cat_vs_cat_chart2"),
          fluidRow(
            column(width = 6,
                   plotOutput("cat_vs_cat_chart"),
            ),
            column(width = 6, #offset = 2,
                   plotOutput("cat_vs_cat_chart2"),
            )
          ),
          
          selectInput("numeric_column", "Choose numeric column to compare", choices=NULL, selected=NULL),
          fluidRow(
            column(width = 6,
                   #plotlyOutput("cat_vs_num_chart"),
                   plotOutput("cat_vs_num_chart"),
            ),
            column(width = 6, #offset = 2,
                   plotOutput("density_chart"),
            ),
            #plotOutput("facet_chart"),
          ),
          "For complex feature compare (facet_wrap)",
          plotOutput("complex_chart"),
      ),
      ### MODEL INFORMATION ----
      tabPanel("Model information",
         fluidRow(
           column(width=6,
                  "Variable important H2O model:",
                  plotOutput("variable_important"),
                  
           ),
           column(width=6,
                  "Shap summary plot via test set:",
                  plotOutput("shap_summary_plot"),
                  
           ),
         ),     
          selectInput("all_column", "Choose column for PDP and ICE plot explaination", choices=NULL, selected=NULL),
          fluidRow(
            column(width=6,
                   "Partial Dependence (PD) Plots:",
                   plotOutput("partial_dependence_plot"),
                   
            ),
            column(width=6,
                   "Individual Conditional Expectiation (ICE) Plots:",
                   plotOutput("Individual_Conditional_Expectiation_plot"),
            ),
          ),
          fluidRow(
            column(width=6,
                   "H2O Performance",
                   verbatimTextOutput("h2o_performance"),
                   
            ),
            column(width=6,
                   "Model confusion matrix:",
                   verbatimTextOutput("confusionmatrix"),
                   
            ),
          ),
      ),
      ### PREDICTION ----
      tabPanel("Prediction",
          "Show all test set:",
          dataTableOutput("test_set_DT"),
          "Predict test set",
          numericInput("obs", "Please choose row on test set for prediction",value = 10,min = 1, max = 100),
          "Show test row choose",
          tableOutput("test_row_choose"), # Show test set choose all column
          "Show test row choose target label",
          tableOutput("test_row_choose_target"),
          "Prediction row choose",
          tableOutput("test_predict"),
          
          fluidRow(
            column(width=6,
                   "Explain each row on test set by shap h2o: ",
                   plotOutput("Shap_Explain_Row_plot"),
            ),
            column(width=6,
                   "Explain each row on test set by LIME h2o: ",
                   plotOutput("Lime_Explain_Row_plot"),
            ),
          ),
          h4("Strategy to retain employee: "),
          verbatimTextOutput("strategy_retain_cust"),
          
      )
    )#end mainpanel
)# end sidebarlayout
