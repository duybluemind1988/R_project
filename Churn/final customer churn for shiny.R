# 1. Library ---- 

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
# EDA
library(correlationfunnel)

# Core & Data Viz
library(data.table)
library(tidyverse)
library(plotly)
library(tidyquant)

#2. Get data ----

getwd()
data_raw <- fread("Churn/WA_Fn-UseC_-Telco-Customer-Churn.csv")

#Remove unnecessary data
data <- data_raw %>%
  drop_na() %>%
  select(Churn, everything())


#3. EDA -----

## Plot categorical vs catagorical
Categorical_vs_categorical_plot <- function(data,group_col,fill_col){
  data %>%
    group_by_(group_col, fill_col) %>%
    summarize(n = n()) %>% 
    mutate(pct = n/sum(n),lbl = scales::percent(pct))%>% 
    ggplot(aes_(x = group_col,y = ~pct,
                fill = fill_col)) +
    geom_bar(stat = "identity",
             position = "fill") +
    scale_y_continuous(breaks = seq(0, 1, .2),label =scales::percent) +
    geom_text(aes(label = lbl), 
              size = 3, 
              position = position_stack(vjust = 0.5)) +
    #scale_fill_brewer(palette = "Set2") +
    labs(y = "Percent",x = "Attrition",title = "Compare attrition accross category")+
    theme_minimal()  
  
}
Categorical_vs_categorical_plot(data,~Churn,~TotalCharges)

## Plot Categorical vs. Quantitative
Categorical_vs_quantitative_plot <- function(data,categorical_col,quantitative_col){
  # plot the distribution using violin and boxplots
  ggplot(data, aes_(x = categorical_col, 
                    y = quantitative_col)) +
    geom_violin(fill = "cornflowerblue") +
    geom_boxplot(width = .2, 
                 fill = "orange",
                 outlier.color = "orange",
                 outlier.size = 2) 
}
Categorical_vs_quantitative_plot(data,~Churn,~tenure)

## density plot
density_plot <-function(data,categorical_col,quantitative_col ){
  ggplot(data, 
         aes_(x = quantitative_col, fill = categorical_col)) + 
    geom_density(alpha = 0.7) + 
    scale_fill_manual(values = c("#386cb0","#fdb462"))
}
density_plot(data,~Churn,~tenure)

## Plot combine
facet_plot <- function(data,categorical_col,quantitative_col,facet_col){
  # plot the distribution using violin and boxplots
  ggplot(data, aes_(x = categorical_col, 
                    y = quantitative_col)) +
    geom_violin(fill = "cornflowerblue") +
    geom_boxplot(width = .2, 
                 fill = "orange",
                 outlier.color = "orange",
                 outlier.size = 2) +
    facet_wrap(facet_col) +
    labs(title=facet_col)
}

facet_plot(data,~Churn,~tenure,~Contract)

#4. Survival analysis ----
## Process data (not convert contract) ----
train_tbl <- data %>%
  mutate(
    Churn_Yes                     = Churn == "Yes",
    OnlineSecurity_No             = OnlineSecurity == "No",
    TechSupport_No                = TechSupport == "No",
    InternetService_FiberOptic    = InternetService %>% str_detect("Fiber"),
    PaymentMethod_ElectronicCheck = PaymentMethod %>% str_detect("Electronic"),
    OnlineBackup_No               = OnlineBackup == "No",
    DeviceProtection_No           = DeviceProtection == "No"
  ) %>%
  select(
    tenure, Churn_Yes, Contract, OnlineSecurity_No, TechSupport_No, InternetService_FiberOptic,
    PaymentMethod_ElectronicCheck, OnlineSecurity_No, DeviceProtection_No
  ) 
train_tbl

## Survival Tables (Kaplan-Meier Method) ----
survfit_simple <- survfit(Surv(tenure, Churn_Yes) ~ strata(Contract), data = train_tbl)
survfit_simple
tidy(survfit_simple)

## Cox Regression Model (Multivariate) ----
model_coxph <- coxph(Surv(tenure, Churn_Yes) ~ . - Contract + strata(Contract), data = train_tbl)
model_coxph
tidy(model_coxph) ## Regression Estimates
survfit_coxph <- survfit(model_coxph)

model_coxph %>% # Mortality Table
  survfit() %>%
  tidy()

## Survival curve ----

plot_customer_survival <- function(object_survfit) {
  
  g <- tidy(object_survfit) %>%
    ggplot(aes(time, estimate, color = strata)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
    geom_line(size = 1) +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Churn Problem", color = "Contract Type", 
         x = "Days After Purchase", y = "Percentage of Customers Staying")
  
  ggplotly(g)
}

plot_customer_survival(survfit_simple)
plot_customer_survival(survfit_coxph)

plot_customer_loss <- function(object_survfit) {
  
  g <- tidy(object_survfit) %>%
    mutate(customers_lost = 1 - estimate) %>%
    ggplot(aes(time, customers_lost, color = strata)) +
    geom_line(size = 1) +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Churn Problem", color = "Contract Type", 
         x = "Days After Purchase", y = "Percentage of Customers Lost")
  
  ggplotly(g)
}
plot_customer_loss(survfit_simple)
plot_customer_loss(survfit_coxph)

#5. Predict with survival model ----
## Cox PH - Produces Theoretical Hazard Ratio ----
table_predict <- predict(model_coxph, newdata = train_tbl, type = "expected") %>%
          tibble(.pred = .) %>%
          bind_cols(train_tbl) # predict Yes/No churn with propability
table_predict<-table_predict  %>%
            mutate(prediction=ifelse(.pred>0.5,"TRUE","FALSE"))

str(table_predict)
predict <- factor(table_predict$prediction)
reference <- factor(table_predict$Churn_Yes)
caret::confusionMatrix(predict,reference,positive="TRUE",mode="prec_recall")
# Precision : 0.38389
# Recall : 0.26271
# F1 : 0.31194
# Balanced Accuracy : 0.55504

## Survival Regression w/ Parsnip ----
model_survreg <- parsnip::surv_reg(mode = "regression", dist = "weibull") %>%
  #set_engine("survreg", control = survreg.control(maxiter=500)) %>% # survreg not available
  set_engine("survival", control = survreg.control(maxiter=500)) %>%
  fit.model_spec(Surv(I(tenure + 1), Churn_Yes) ~ . - Contract + strata(Contract), data = train_tbl)

model_survreg$fit %>% tidy()

predict(model_survreg, new_data = train_tbl) %>%
  bind_cols(train_tbl %>% select(Churn_Yes, everything())) # predict day churn

#6. Prepare data for build ML model ----
response="Churn"
set.seed(430)
split = caret::createDataPartition(data[[response]], p =0.6, list = FALSE)
train = data[split, ]

valid_test = data[-split, ]
split2 = caret::createDataPartition(valid_test[[response]], p =0.5, list = FALSE)
valid = valid_test[split2, ]
test = valid_test[-split2, ]

dim(train)
dim(valid)
dim(test)
prop.table(table(as.data.frame(train[[response]])))
prop.table(table(as.data.frame(valid[[response]])))
prop.table(table(as.data.frame(test[[response]])))

recipe_obj <- recipe(Churn ~ ., data = train) %>%
  step_rm(customerID) %>% 
  step_discretize(tenure, options = list(cuts = 6)) %>% 
  step_string2factor(all_nominal()) %>% # convert character to factor (auto convert char to factor with recipes)
  step_nzv(all_numeric(), -all_outcomes())  %>% #Remove near-zero variance features like sex, yes/no...
  prep()

baked_train <- bake(recipe_obj, new_data = train)
baked_valid <- bake(recipe_obj, new_data = valid)
baked_test <- bake(recipe_obj, new_data = test)
# convert training data to h2o object
train_h2o <- as.h2o(baked_train)
valid_h2o <- as.h2o(baked_valid)
test_h2o <- as.h2o(baked_test)
# set the predictor names
predictors <- setdiff(colnames(baked_train), response)

#7. ML model ----

##  GBM BEST ----
assignment_type <- "Stratified"
tic()
h2o.gbm <- h2o.gbm(
  x = predictors, 
  y = response,
  training_frame = train_h2o, 
  #validation_frame = valid_h2o,
  balance_classes = TRUE,
  fold_assignment = assignment_type,
  nfolds=5,
  #ntrees = 1000,
  #booster = "dart",
  #normalize_type = "tree",
  
  seed = 123
)
toc() # 7 s
test_pred <-h2o.predict(h2o.gbm, newdata = test_h2o)
predict <- as.data.frame(test_pred)$predict
reference <- as.data.frame(test_h2o)[[response]]
caret::confusionMatrix(predict,reference,positive = "Yes",mode="prec_recall")

#Precision : 0.5406          
#Recall : 0.8204          
#F1 : 0.6518

## Random forest ----
assignment_type <- "Stratified"
tic()
h2o_model_rf <- h2o.randomForest(
  x = predictors, 
  y = response,
  training_frame = train_h2o, 
  validation_frame = valid_h2o,
  balance_classes = TRUE,
  ntrees = 1000,
  #fold_assignment = assignment_type,
  #nfolds=5,
  seed = 123
)
toc() # 7 s
test_pred <-h2o.predict(h2o_model_rf, newdata = test_h2o)
predict <- as.data.frame(test_pred)$predict
reference <- as.data.frame(test_h2o)[[response]]
caret::confusionMatrix(predict,reference,positive = "Yes",mode="prec_recall")
#Precision : 0.5577          
#Recall : 0.7641          
#F1 : 0.6448   

## Deep learning ----
model_dnn <- h2o.deeplearning(x = predictors, 
                              y = response, 
                              training_frame = train_h2o,
                              validation_frame=valid_h2o,
                              hidden = c(200,200), # c(200,200)
                              epoch = 100, # 10
                              model_id = "baseline_dnn", 
                              #nfolds = 5, 
                              stopping_metric="AUC", ## could be "MSE","logloss","r2","AUC", "AUCPR", "mean_per_class_error","custom"
                              stopping_tolerance=0.01,  ## stop when misclassification does not improve by >=1% for 2 scoring events
                              seed = 1234)

test_pred <-h2o.predict(model_dnn, newdata = test_h2o)
predict <- as.data.frame(test_pred)$predict
reference <- as.data.frame(test_h2o)[[response]]
caret::confusionMatrix(predict,reference,positive = "Yes",mode="prec_recall")
#Precision : 0.5418          
#Recall : 0.7641         
#F1 : 0.6340

# 8. Save model ----
# Save recipe
write_rds(recipe_obj,"Churn/recipe.Rds")

#Save model
slect_model=h2o.gbm
getwd()
model_path <- h2o.saveModel(object = slect_model, path = "Churn/", force = TRUE)
print(model_path)
# /home/dnn/Data_science/Git/R_project/Churn/GBM_model_R_1613740880034_453

# 9. Load model ----
# Load recipe
recipe_load <- readr::read_rds("Churn/recipe.Rds")
# Load model
model_path <- "Churn/GBM_model_R_1613740880034_453"
h2o_model_load <- h2o.loadModel(model_path)

#10. Explain model ----
dev.off() # clear all plot to avoid call graphics error
## Explain GBM model ----
h2o.varimp(h2o_model_load)
h2o.varimp_plot(h2o_model_load)
h2o.shap_summary_plot(h2o_model_load,test_h2o)
h2o.pd_plot(h2o_model_load, test_h2o, column = "Contract")
h2o.pd_plot(h2o_model_load, test_h2o, column = "tenure")
h2o.pd_plot(h2o_model_load, test_h2o, column = "MonthlyCharges")
h2o.pd_plot(h2o_model_load, test_h2o, column = "OnlineSecurity")
h2o.pd_plot(h2o_model_load, test_h2o, column = "PaymentMethod")
h2o.pd_plot(h2o_model_load, test_h2o, column = "MultipleLines")
h2o.partialPlot(h2o_model_load, data = test_h2o, cols = "MonthlyCharges")
h2o.ice_plot(h2o_model_load, test_h2o, column = "MonthlyCharges")

# Class = No
h2o.shap_explain_row_plot(h2o_model_load, test_h2o,row_index = 10)
h2o.shap_explain_row_plot(h2o_model_load, test_h2o,row_index = 3)
# Class =Yes #2,17,20
h2o.shap_explain_row_plot(h2o_model_load, test_h2o,row_index = 18)
h2o.shap_explain_row_plot(h2o_model_load, test_h2o,row_index = 4)

# Run lime() on training set
explainer <- lime::lime(
  as.data.frame(train_h2o), 
  model          = h2o_model_load, 
  bin_continuous = FALSE)
# Run explain() on explainer
explanation <- lime::explain(
  as.data.frame(test_h2o[1,]), 
  explainer    = explainer, 
  n_labels     = 1, 
  n_features   = 10,
  kernel_width = 0.5)

lime::plot_features(explanation) +
  labs(title = "HR Predictive Analytics: LIME Feature Importance Visualization",
       subtitle = "Hold Out (Test) Set, First 10 Cases Shown")
plot_explanations(explanation)

explanation_caret <- explain(
  x = as.data.frame(test_h2o[1:5,]), 
  explainer = explainer, 
  n_permutations = 5000,
  dist_fun = "gower",
  kernel_width = .75,
  n_features = 10, 
  #feature_select = "highest_weights",
  labels = "Yes"
)
plot_features(explanation_caret)
plot_explanations(explanation_caret)

## Explain combine model ----
explainer_h2o_rf  <- lime(as.data.frame(train_h2o), h2o_model_rf, n_bins = 5)
explainer_h2o_gbm <- lime(as.data.frame(train_h2o), h2o.gbm, n_bins = 5)

explanation_rf  <- lime::explain(as.data.frame(test_h2o[1:5,]), 
                                 explainer_h2o_rf, 
                                 n_features      = 5, 
                                 labels          = "Yes", 
                                 kernel_width    = .1, 
                                 feature_select  = "highest_weights")
explanation_gbm <- lime::explain(as.data.frame(test_h2o[1:5,]), 
                                 explainer_h2o_gbm, 
                                 n_features      = 5, 
                                 labels          = "Yes", 
                                 kernel_width    = .1, 
                                 feature_select  = "highest_weights")
p1 <- plot_features(explanation_rf,  ncol = 1) + ggtitle("rf")
p2 <- plot_features(explanation_gbm, ncol = 1) + ggtitle("gbm")
gridExtra::grid.arrange(p1, p2, nrow = 1)

## Explain DL model ----
h2o.varimp_plot(model_dnn)
h2o.pd_plot(model_dnn, test_h2o, column = "Contract")
h2o.pd_plot(h2o_model_load, test_h2o, column = "tenure")
h2o.partialPlot(model_dnn, data = test_h2o, cols = "Contract")
h2o.ice_plot(model_dnn, test_h2o, column = "Contract")

# Problem: 
# Class = No
h2o.shap_explain_row_plot(model_dnn, test_h2o,row_index = 10)
h2o.shap_explain_row_plot(model_dnn, test_h2o,row_index = 3)
# Class =Yes #2,17,20
h2o.shap_explain_row_plot(model_dnn, test_h2o,row_index = 18)
h2o.shap_explain_row_plot(model_dnn, test_h2o,row_index = 4)
