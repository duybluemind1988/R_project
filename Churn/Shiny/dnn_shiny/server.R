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

path<-'/home/dnn/Data_science/Git/R_project/Churn/WA_Fn-UseC_-Telco-Customer-Churn.csv'
# load the model
model_path <- "/home/dnn/Data_science/Git/R_project/Churn/Shiny/dnn_shiny/GBM_model_R_1613740880034_453"
recipe_path <- "/home/dnn/Data_science/Git/R_project/Churn/Shiny/dnn_shiny/recipe.Rds"

h2o_model_load <- h2o.loadModel(model_path)
recipe_load <- readr::read_rds(recipe_path)

server <- function(input, output, session) {
  
  data <- reactive({fread(path)})
  
  output$data_head_DT<-renderDataTable(data(),
                                       options =list(pageLength = 5)
  )
  
}