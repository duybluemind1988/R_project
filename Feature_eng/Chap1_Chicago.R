library(caret)
library(sessioninfo)
library(xgboost)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 2)
registerDoParallel(cl)

# ------------------------------------------------------------------------------

# Load pre-made data
load("../../Data_Sets/Chicago_trains/chicago.RData")

# ------------------------------------------------------------------------------


rand_ctrl <- ctrl
rand_ctrl$search <- "random"

# ------------------------------------------------------------------------------

pred_vars <- c(var_sets$dates, var_sets$lag14)

set.seed(4194)
boost_date_lag14 <- train(s_40380 ~ .,
                          data = training[, c("s_40380", pred_vars)],
                          method = "xgbTree",
                          tuneLength = 50,
                          metric = "RMSE",
                          maximize = FALSE,
                          trControl = rand_ctrl)
save(boost_date_lag14, file = "boost_date_lag14.RData")

# ------------------------------------------------------------------------------

pred_vars <- c(var_sets$dates, var_sets$lag14, var_sets$holidays)

set.seed(4194)
boost_date_lag14_hol <- train(s_40380 ~ .,
                              data = training[, c("s_40380", pred_vars)],
                              method = "xgbTree",
                              tuneLength = 50,
                              metric = "RMSE",
                              maximize = FALSE,
                              trControl = rand_ctrl)
save(boost_date_lag14_hol, file = "boost_date_lag14_hol.RData")

# ------------------------------------------------------------------------------

pred_vars <- c(var_sets$dates, var_sets$lag14, var_sets$weather)

set.seed(4194)
boost_date_lag14_weth <- train(s_40380 ~ .,
                               data = training[, c("s_40380", pred_vars)],
                               method = "xgbTree",
                               tuneLength = 50,
                               metric = "RMSE",
                               maximize = FALSE,
                               trControl = rand_ctrl)
save(boost_date_lag14_weth, file = "boost_date_lag14_weth.RData")

# ------------------------------------------------------------------------------

pred_vars <- c(var_sets$dates, var_sets$lag14, var_sets$other_lag)

set.seed(4194)
boost_date_lagall <- train(s_40380 ~ .,
                           data = training[, c("s_40380", pred_vars)],
                           method = "xgbTree",
                           tuneLength = 50,
                           metric = "RMSE",
                           maximize = FALSE,
                           trControl = rand_ctrl)
save(boost_date_lagall, file = "boost_date_lagall.RData")

# ------------------------------------------------------------------------------

set.seed(4194)
boost_date_only <- train(s_40380 ~ .,
                         data = training[, c("s_40380", var_sets$dates)],
                         method = "xgbTree",
                         tuneLength = 50,
                         metric = "RMSE",
                         maximize = FALSE,
                         trControl = rand_ctrl)
save(boost_date_only, file = "boost_date_only.RData")


