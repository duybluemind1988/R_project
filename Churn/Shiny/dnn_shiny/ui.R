library(shiny)
ui <- fluidPage(
  navbarPage("Churn prediction",
            tabPanel("Explore data",
                      h4("1. All data: "),
                      dataTableOutput("data_head_DT"),
                      h4("Correlation funnel:"),
                      plotlyOutput("correlation_funnel"), 
                      h4("2. Compare categorical features"),
                      selectInput("category_column1", "Choose category column to compare", choices=NULL, selected=NULL),
                      selectInput("category_column2", "Choose category column for facet", choices=NULL, selected=NULL),
                      fluidRow(
                        column(width = 4,
                               plotOutput("cat_vs_cat_chart2"),
                        ),
                        column(width = 8, #offset = 2,
                               plotOutput("Categorical_facet_plot"),
                        )
                      ),
                      
                      h4("3. Compare numeric features"),
                      selectInput("numeric_column", "Choose numeric column to compare", choices=NULL, selected=NULL),
                      selectInput("category_column3", "Choose column for facet_wrap", choices=NULL, selected=NULL),
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
                      h4("4. facet_wrap comparision"),
                      plotOutput("facet_chart"),
                      #selectInput("numeric_column2", "Choose numeric column to compare", choices=NULL, selected=NULL),
                     
                      
                      
            ),
            tabPanel("Survival analysis",
                      selectInput("category_column4", "Choose column for survival plot", choices=NULL, selected=NULL),
                      
                      fluidRow(
                        column(width = 6,
                               h4("Kaplan Meier information"),
                               dataTableOutput("KM_model_table"),
                               "Kaplan Meier tidy",
                               verbatimTextOutput("KM_model_survfit"),
                               
                        ),
                        column(width = 6, #offset = 2,
                               h4("Model coxph information"),
                               dataTableOutput("coxph_model_table"),
                               "Model coxph survfit",
                               verbatimTextOutput("coxph_model_survfit"),
                               verbatimTextOutput("coxph_model_infor"),
                               
                        ) 
                      ),
                      fluidRow(
                        column(width=6,
                               plotlyOutput("KM_plot_survival"),
                               plotlyOutput("KM_plot_loss"),
                        ),
                        column(width=6,
                               plotlyOutput("coxph_plot_survival"),
                               plotlyOutput("coxph_plot_loss"),
                        ),
                      ),
            ),
            tabPanel("Model explain",
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
            tabPanel("Prediction",
                     "Show all testset for prediction:",
                     dataTableOutput("test_set_DT"),
                     selectInput("custome_id", "Select customer ID for prediction", choices=NULL, selected=NULL),
                     "Show test row choose",
                     dataTableOutput("test_row_choose_DT"), # Show test set choose all column
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

            )
  )
)  




