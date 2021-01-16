# BUSINESS SCIENCE ----
# LEARNING LAB 07 - TIME SERIES FORECASTING ----
# Data Used is a modified version of the UCI Machine Learning Bike Sharing Data Set
# Data Source: https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset

# 1.0 GOALS ----
# - Predict next 92-days (3-MONTHS) of daily bike sharing

# 2.0 LIBRARIES ----

# Main Packages
library(tidyverse)
library(lubridate)
library(tidyquant)
library(plotly)

# Time Based Feature Extraction
library(timetk)

# Holidays
library(timeDate)

# Weather Data
library(riem)

# Forecasting
library(forecast)
library(sweep)

# Machine Learning 
# - DS4B 101-R Week 6 
# - DS4B 201-R Weeks 5, 6, 7 (H2O & Lime)
library(parsnip)
library(yardstick)


# 3.0 DATA IMPORT ----
# - Taught in DS4B 101-R: Weeks 2 (readr & odbc)

sharing_d_tbl <- read_csv("00_data/sharing_d_tbl.csv") 

sharing_d_tbl


# 2.0 VISUALIZATION ----
# - Taught in DS4B 101-R: Week 4 (ggplot2), Week 7 (plotly)

g <- sharing_d_tbl %>%
    ggplot(aes(date, cnt)) +
    geom_line(alpha = 0.5, color = "#2c3e50") +
    geom_smooth(method = "loess", span = 0.5) +
    theme_tq() 

ggplotly(g) %>%
    layout(xaxis = list(rangeslider = list(type = "date")))

# 3.0 HYPOTHESIS ----
# - Bike Sharing is related to:
#   - Growth Trend
#   - Seasonality (aka Time of the year)
#   - Holidays
#   - Weather
#   - Extreme Events


# 4.0 ALTERNATIVE DATA ----

# 4.1 Time-based Features ----
sharing_d_tbl <- sharing_d_tbl %>%
    tk_augment_timeseries_signature() %>%
    select(date, cnt, index.num, year, half, quarter, month.lbl, day, wday.lbl)

sharing_d_tbl

# 4.2 Holidays ----
holidays <- holidayNYSE(year = c(2011, 2012)) %>% ymd()

sharing_d_tbl <- sharing_d_tbl %>%
    mutate(holiday = case_when(
        date %in% holidays ~ 1,
        TRUE ~ 0
    ))

sharing_d_tbl

# 4.3 Weather Data for DC ----
# - reim package: http://ropensci.github.io/riem/index.html
# - Map: https://mesonet.agron.iastate.edu/request/download.phtml?network=VA_ASOS

# VA Weather Stations, Use DCA (See Map)
riem::riem_stations("VA_ASOS") 
weather_dc_tbl <- riem_measures("DCA", date_start = "2011-01-01", date_end = "2013-01-01")

weather_dc_tbl 

weather_dc_tbl %>% glimpse()

# Data is Messy! 
# - Learn Data Cleaning & Manipulation in 101 Weeks 2 & 3 
# - dplyr, tidyr, lubridate, stringr, forcats
weather_dc_tbl %>%
    select(valid, tmpf, dwpf, relh, sknt, vsby, skyc1, peak_wind_gust, feel) %>%
    map_df(~ sum(is.na(.)))

weather_dc_d_tbl <- weather_dc_tbl %>%
    select(valid, tmpf, dwpf, relh, sknt, vsby, peak_wind_gust, feel) %>%
    mutate(date = as_date(valid)) %>%
    select(-valid) %>%
    group_by(date) %>%
    summarize_all(~ median(., na.rm = TRUE)) %>%
    fill(peak_wind_gust, .direction = "down") %>%
    fill(peak_wind_gust, .direction = "up") 

weather_dc_d_tbl

sharing_d_tbl <- sharing_d_tbl %>%
    left_join(weather_dc_d_tbl, by = "date") 

sharing_d_tbl

# 4.0 INVESIGATE AUTOCORRELATION ----

autocorrelate <- function(data, value, lags = 0:20) {
    
    value_expr <- enquo(value)
    
    acf_values <- data %>%
        select(!! value_expr) %>%
        pull() %>%
        acf(lag.max = tail(lags, 1), plot = FALSE) %>%
        .$acf %>%
        .[,,1]
    
    ret <- tibble(acf = acf_values) %>%
        rowid_to_column(var = "lag") %>%
        mutate(lag = lag - 1) %>%
        filter(lag %in% lags)
    
    return(ret)
}

g <- sharing_d_tbl %>%
    autocorrelate(cnt, lags = 0:nrow(.)) %>%
    ggplot(aes(lag, acf)) +
    geom_point(alpha = 0.5, color = "#2c3e50") +
    expand_limits(y = c(-1, 1)) +
    theme_tq() +
    labs(title = "Autocorrelation")

ggplotly(g)



# 5.0 TRAIN/TEST SPLIT ----
# - Predict next 92-days (3-MONTHS) of daily bike sharing

train_test_split_date <- "2012-10-01"

train_tbl <- sharing_d_tbl %>%
    filter(date < ymd(train_test_split_date))

test_tbl <- sharing_d_tbl %>%
    filter(date >= ymd(train_test_split_date))

# What Autocorrelation can we use in a multivariate model?

nrow(test_tbl)


# 6.0 EVALUATE CURRENT SYSTEM ----
# - 60-Day moving average

moving_average_train_tbl <- train_tbl %>%
    select(date, cnt) %>%
    mutate(moving_average = rollmean(cnt, k = 92, na.pad = TRUE, align = "right")) 

g <- moving_average_train_tbl %>%
    bind_rows(test_tbl %>% select(date, cnt)) %>%
    fill(moving_average, .direction = "down") %>%
    ggplot(aes(date, cnt)) +
    geom_vline(xintercept = ymd(train_test_split_date), color = "red") +
    geom_point(color = "#2c3e50") +
    geom_line(aes(y = moving_average), size = 1, color = "blue") +
    theme_tq() 

ggplotly(g)

test_tbl %>%
    select(cnt) %>%
    mutate(moving_average = moving_average_train_tbl %>% 
               tail(1) %>% 
               pull(moving_average)) %>%
    mae(cnt, moving_average)



# 7.0 MODELING ----

# 7.1 ARIMA ----

### NOT RUN - Takes about 5 minutes
# fit_arima <- train_tbl %>%
#     tk_ts(select = cnt, frequency = 364) %>%
#     auto.arima(stepwise = FALSE, parallel = TRUE, num.cores = 4)
# 
# fit_arima <- train_tbl %>%
#     tk_ts(select = cnt, frequency = 364) %>%
#     Arima(order = c(1, 0, 2),
#           seasonal = c(0, 1, 0),
#           include.drift = TRUE)
# 
# fcast_arima_tbl <- forecast(fit_arima, h = nrow(test_tbl)) %>%
#     sw_sweep(timetk_idx = TRUE, rename_index = "date")

### SAVE RESULTS
# fs::dir_create("00_model")
# fcast_tbl %>% write_rds("00_model/fcast_arima_tbl.rds")

fcast_arima_tbl <- read_rds("00_model/fcast_arima_tbl.rds")

g <- fcast_arima_tbl %>%
    ggplot(aes(date, cnt, color = key)) +
    geom_point(data = test_tbl %>% mutate(key = "actual")) +
    geom_point(alpha = 0.5) +
    theme_tq() +
    scale_color_tq()+
    labs(title = "ARIMA(1,0,2)(0,1,0)[364] with drift")
    
ggplotly(g) %>%
    layout(xaxis = list(rangeslider = list(type = "date")))

fcast_arima_tbl %>% 
    filter(key == "forecast") %>%
    rename(.pred = cnt) %>%
    select(date, .pred) %>%
    left_join(test_tbl) %>%
    select(date:cnt) %>%
    mae(truth = cnt, estimate = .pred)


# 7.2.2 GLMNET ----
library(glmnet)
glmnet_fit <- linear_reg(mode = "regression", penalty = 0.01, mixture = 0.5) %>%
    set_engine("glmnet") %>%
    fit.model_spec(cnt ~ . - date, data = sharing_d_tbl)

fcast_glmnet_tbl <- glmnet_fit %>%
    predict(new_data = test_tbl) %>%
    bind_cols(test_tbl) %>%
    bind_rows(train_tbl) %>%
    arrange(date) %>%
    select(date, cnt, .pred) %>%
    gather(key = "key", value = "cnt", -date) %>%
    filter(!is.na(cnt)) %>%
    mutate(key = case_when(
        key == "cnt" ~ "actual",
        TRUE ~ "forecast"
    ))

g <- fcast_glmnet_tbl %>%
    ggplot(aes(date, cnt, color = key)) +
    # geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
    #             color = "white", fill = "dodgerblue") +
    geom_point(data = test_tbl %>% mutate(key = "actual")) +
    geom_point(alpha = 0.5) +
    theme_tq() +
    scale_color_tq()+
    labs(title = "GLMNET: Penalty = 0.01, Mixture = 0.5")

ggplotly(g) %>%
    layout(xaxis = list(rangeslider = list(type = "date")))

fcast_glmnet_tbl %>%
    filter(key == "forecast") %>%
    rename(.pred = cnt) %>%
    select(date, .pred) %>%
    left_join(test_tbl) %>%
    select(date:cnt) %>%
    mae(truth = cnt, estimate = .pred)


# 8.0 STACKING ----

fcast_arima_tbl %>%
    select(date:cnt) %>%
    left_join(fcast_glmnet_tbl, by = c("date", "key")) %>%
    rename(cnt_arima = cnt.x, cnt_glmnet = cnt.y) %>%
    filter(key == "forecast") %>%
    mutate(cnt_stacked = (cnt_arima + cnt_glmnet) / 2) %>%
    left_join(test_tbl %>% select(date, cnt), by = "date") %>%
    mae(cnt, cnt_stacked)

fcast_arima_tbl
