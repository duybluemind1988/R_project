library(parsnip)
library(xgboost)
library(recipes)
library(DataExplorer)
library(tictoc)

tic()
train_tbl <- application_mean_days_credit_tbl

# 2.0 MISSING DATA ----
missing_thresh <- 0.20
missing_names_to_remove <- train_tbl %>%
    DataExplorer::profile_missing() %>%
    arrange(desc(pct_missing)) %>%
    filter(pct_missing > missing_thresh) %>%
    pull(feature) %>%
    as.character()

missing_names_to_remove

missing_names_to_impute <- train_tbl %>%
    select(-missing_names_to_remove) %>%
    DataExplorer::profile_missing() %>%
    arrange(desc(pct_missing)) %>%
    filter(pct_missing > 0) %>%
    pull(feature) %>%
    as.character()

missing_names_to_impute

# 3.0 DATA PREPROCESSING ----
rec_obj <- recipe(TARGET ~ ., data = train_tbl) %>%
    step_rm(SK_ID_CURR) %>%
    step_rm(missing_names_to_remove) %>%
    # step_bagimpute(missing_names_to_impute) %>%
    step_meanimpute(all_numeric()) %>%
    step_modeimpute(all_nominal()) %>%
    step_nzv(all_predictors()) %>%
    #step_num2factor(TARGET) %>% 
    step_mutate_at(TARGET,fn=factor) %>% # replace for step_num2factor
    prep()

rec_obj

train_processed_tbl <- bake(rec_obj, train_tbl)

# 4.0 MODELING ----


model_xgb <- boost_tree(
    mode = "classification", 
    mtry = 50, 
    trees = 5, 
    min_n = 3, 
    tree_depth = 8, 
    learn_rate = 0.1, 
    loss_reduction = 0.1) %>%
    set_engine(engine = "xgboost") %>%
    fit.model_spec(TARGET ~ ., data = train_processed_tbl)
toc()


