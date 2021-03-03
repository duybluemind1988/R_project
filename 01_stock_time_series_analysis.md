

```r
# LEARNING LAB 22: ADVANCED SQL ----
# PART 1: STOCK ANALYSIS - SQL TIME SERIES TRAINING ----
#remotes::install_github("edgararuiz/connections")

hair ="/home/dnn/Data_science/Git/R_project/R database/Lab 22 sql advanced/01_stock_time_series_analysis.R"
knitr::spin( hair,
             knit = TRUE,
             report = TRUE,
             text = NULL,
             envir = parent.frame(),
             format = c("Rmd") )
```

```
## 
## 
## processing file: /home/dnn/Data_science/Git/R_project/R database/Lab 22 sql advanced/01_stock_time_series_analysis.Rmd
```

```
##   |                                                                                                                                                         |                                                                                                                                                 |   0%  |                                                                                                                                                         |................................................                                                                                                 |  33%
##   ordinary text without R code
## 
##   |                                                                                                                                                         |.................................................................................................                                                |  67%
## label: unnamed-chunk-2
##   |                                                                                                                                                         |.................................................................................................................................................| 100%
##   ordinary text without R code
```

```
## output file: 01_stock_time_series_analysis.md
```

```
## Warning in file.remove(outsrc): cannot remove file '/home/dnn/Data_science/Git/R_project/R
## database/Lab 22 sql advanced/01_stock_time_series_analysis.Rmd', reason 'No such file or directory'
```

```r
library(tidyverse)
library(lubridate)
library(tidyquant)
library(plotly)

library(dbplyr)

library(DBI)
library(RSQLite)
library(connections) # remotes::install_github("edgararuiz/connections")

con_stocks <- connection_open(drv = SQLite(), dbname =
                                  "/home/dnn/Data_science/Git/Business science/Database-SQL/lab_22_sql_advanced - llpro/lab_22_sql_advanced - llpro/stocks.sqlite")


# DATA ----
g <- tbl(con_stocks, "stock_history") %>%
    collect() %>%
    
    mutate(date = ymd(date)) %>%
    
    ggplot(aes(date, adjusted, color = symbol)) +
    geom_line() +
    theme_tq() +
    scale_color_tq()

ggplotly(g)
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Error in path.expand(path): invalid 'path' argument
```

```r
# 1.0 LAGS & DIFFS ---- 

# Prepare SQL Query w/ dbplyr
lags_diffs_query <- tbl(con_stocks, "stock_history") %>%
    
    select(symbol, date, adjusted) %>%
    
    group_by(symbol) %>%
    
    window_order(date) %>%
    
    mutate(lag_1      = lag(adjusted, n = 1)) %>%
    mutate(diff_1     = adjusted - lag_1) %>%
    mutate(pct_diff_1 = diff_1 / lag_1) %>%
    mutate(growth     = cumsum(pct_diff_1) + 1) %>%
    
    ungroup() %>%
    filter(!is.na(lag_1)) 

lags_diffs_query %>% show_query()
```

```
## <SQL>
## SELECT *
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `diff_1`, `pct_diff_1`, SUM(`pct_diff_1`) OVER (PARTITION BY `symbol` ORDER BY `date` ROWS UNBOUNDED PRECEDING) + 1.0 AS `growth`
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `diff_1`, `diff_1` / `lag_1` AS `pct_diff_1`
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `adjusted` - `lag_1` AS `diff_1`
## FROM (SELECT `symbol`, `date`, `adjusted`, LAG(`adjusted`, 1, NULL) OVER (PARTITION BY `symbol` ORDER BY `date`) AS `lag_1`
## FROM (SELECT `symbol`, `date`, `adjusted`
## FROM `stock_history`)))))
## WHERE (NOT(((`lag_1`) IS NULL)))
```

```r
lags_diffs_query
```

```
## [38;5;246m# Source:     lazy query [?? x 7][39m
## [38;5;246m# Database:   sqlite 3.34.1 [/home/dnn/Data_science/Git/Business
## #   science/Database-SQL/lab_22_sql_advanced - llpro/lab_22_sql_advanced - llpro/stocks.sqlite][39m
## [38;5;246m# Ordered by: date[39m
##    symbol date       adjusted lag_1 diff_1 pct_diff_1 growth
##    [3m[38;5;246m<chr>[39m[23m  [3m[38;5;246m<chr>[39m[23m         [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<dbl>[39m[23m  [3m[38;5;246m<dbl>[39m[23m      [3m[38;5;246m<dbl>[39m[23m  [3m[38;5;246m<dbl>[39m[23m
## [38;5;250m 1[39m AAPL   2009-01-05     11.8  11.3  0.477     0.042[4m2[24m  1.04 
## [38;5;250m 2[39m AAPL   2009-01-06     11.6  11.8 -[31m0[39m[31m.[39m[31m194[39m    -[31m0[39m[31m.[39m[31m0[39m[31m16[4m5[24m[39m  1.03 
## [38;5;250m 3[39m AAPL   2009-01-07     11.3  11.6 -[31m0[39m[31m.[39m[31m251[39m    -[31m0[39m[31m.[39m[31m0[39m[31m21[4m6[24m[39m  1.00 
## [38;5;250m 4[39m AAPL   2009-01-08     11.6  11.3  0.211     0.018[4m6[24m  1.02 
## [38;5;250m 5[39m AAPL   2009-01-09     11.3  11.6 -[31m0[39m[31m.[39m[31m264[39m    -[31m0[39m[31m.[39m[31m0[39m[31m22[4m9[24m[39m  1.00 
## [38;5;250m 6[39m AAPL   2009-01-12     11.1  11.3 -[31m0[39m[31m.[39m[31m239[39m    -[31m0[39m[31m.[39m[31m0[39m[31m21[4m2[24m[39m  0.979
## [38;5;250m 7[39m AAPL   2009-01-13     10.9  11.1 -[31m0[39m[31m.[39m[31m118[39m    -[31m0[39m[31m.[39m[31m0[39m[31m10[4m7[24m[39m  0.968
## [38;5;250m 8[39m AAPL   2009-01-14     10.6  10.9 -[31m0[39m[31m.[39m[31m297[39m    -[31m0[39m[31m.[39m[31m0[39m[31m27[4m1[24m[39m  0.941
## [38;5;250m 9[39m AAPL   2009-01-15     10.4  10.6 -[31m0[39m[31m.[39m[31m243[39m    -[31m0[39m[31m.[39m[31m0[39m[31m22[4m9[24m[39m  0.918
## [38;5;250m10[39m AAPL   2009-01-16     10.3  10.4 -[31m0[39m[31m.[39m[31m131[39m    -[31m0[39m[31m.[39m[31m0[39m[31m12[4m6[24m[39m  0.905
## [38;5;246m# … with more rows[39m
```

```r
# Plot Mean Daily Returns
g <- lags_diffs_query %>%
    collect() %>%
    
    ggplot(aes(symbol, pct_diff_1, fill = symbol)) +
    geom_jitter(aes(text = str_glue("Date: {date}
                                    Pct Change: {scales::percent(pct_diff_1)}")), 
                fill = "black", alpha = 0.15) +
    geom_violin(alpha = 0.5) +
    theme_tq() +
    scale_fill_tq()
```

```
## Warning: Ignoring unknown aesthetics: text
```

```r
ggplotly(g, tooltip = "text")
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Error in path.expand(path): invalid 'path' argument
```

```r
# Plot Wealth
g <- lags_diffs_query %>%
    collect() %>%
    
    mutate(date = ymd(date)) %>%
    
    ggplot(aes(date, growth, color = symbol, group = symbol)) +
    geom_line() +
    theme_tq() +
    scale_color_tq()

ggplotly(g)
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Error in path.expand(path): invalid 'path' argument
```

```r
# 2.0 WINDOW (ROLLING) FUNCTIONS ----

rolling_window_query <- tbl(con_stocks, "stock_history") %>%
    
    select(symbol, date, adjusted) %>%
    
    group_by(symbol) %>%
    
    window_frame(from = -90, to = 0) %>%
    window_order(date) %>%
    
    mutate(roll_avg = mean(adjusted, na.rm = TRUE))  %>%
    
    ungroup()

rolling_window_query %>% show_query()
```

```
## <SQL>
## SELECT `symbol`, `date`, `adjusted`, AVG(`adjusted`) OVER (PARTITION BY `symbol` ORDER BY `date` ROWS 90 PRECEDING) AS `roll_avg`
## FROM (SELECT `symbol`, `date`, `adjusted`
## FROM `stock_history`)
```

```r
rolling_window_query
```

```
## [38;5;246m# Source:     lazy query [?? x 4][39m
## [38;5;246m# Database:   sqlite 3.34.1 [/home/dnn/Data_science/Git/Business
## #   science/Database-SQL/lab_22_sql_advanced - llpro/lab_22_sql_advanced - llpro/stocks.sqlite][39m
## [38;5;246m# Ordered by: date[39m
##    symbol date       adjusted roll_avg
##    [3m[38;5;246m<chr>[39m[23m  [3m[38;5;246m<chr>[39m[23m         [3m[38;5;246m<dbl>[39m[23m    [3m[38;5;246m<dbl>[39m[23m
## [38;5;250m 1[39m AAPL   2009-01-02     11.3     11.3
## [38;5;250m 2[39m AAPL   2009-01-05     11.8     11.6
## [38;5;250m 3[39m AAPL   2009-01-06     11.6     11.6
## [38;5;250m 4[39m AAPL   2009-01-07     11.3     11.5
## [38;5;250m 5[39m AAPL   2009-01-08     11.6     11.5
## [38;5;250m 6[39m AAPL   2009-01-09     11.3     11.5
## [38;5;250m 7[39m AAPL   2009-01-12     11.1     11.4
## [38;5;250m 8[39m AAPL   2009-01-13     10.9     11.4
## [38;5;250m 9[39m AAPL   2009-01-14     10.6     11.3
## [38;5;250m10[39m AAPL   2009-01-15     10.4     11.2
## [38;5;246m# … with more rows[39m
```

```r
g <- rolling_window_query %>%
    collect() %>%
    
    mutate(date = ymd(date)) %>%
    
    ggplot(aes(x = date, y = adjusted, color = symbol, group = symbol)) +
    geom_line() +
    geom_line(aes(y = roll_avg), color = "blue") +
    scale_color_tq() +
    theme_tq()

ggplotly(g)
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Error in path.expand(path): invalid 'path' argument
```

```r
# 3.0 TIME-BASED AGGREGATIONS -----

# 3.1 By Month ----
lags_diffs_query %>%
    
    mutate(year_mon = substr(date, start = 1, stop = 7)) %>%
    
    group_by(symbol, year_mon) %>%
    summarize(pct_change = sum(pct_diff_1, na.rm = TRUE)) %>%
    ungroup()
```

```
## `summarise()` has grouped output by 'symbol'. You can override using the `.groups` argument.
```

```
## [38;5;246m# Source:   lazy query [?? x 3][39m
## [38;5;246m# Database: sqlite 3.34.1 [/home/dnn/Data_science/Git/Business
## #   science/Database-SQL/lab_22_sql_advanced - llpro/lab_22_sql_advanced - llpro/stocks.sqlite][39m
##    symbol year_mon pct_change
##    [3m[38;5;246m<chr>[39m[23m  [3m[38;5;246m<chr>[39m[23m         [3m[38;5;246m<dbl>[39m[23m
## [38;5;250m 1[39m AAPL   2009-01     0.002[4m4[24m[4m1[24m
## [38;5;250m 2[39m AAPL   2009-02    -[31m0[39m[31m.[39m[31m00[39m[31m1[4m9[24m[4m6[24m[39m
## [38;5;250m 3[39m AAPL   2009-03     0.173  
## [38;5;250m 4[39m AAPL   2009-04     0.184  
## [38;5;250m 5[39m AAPL   2009-05     0.082[4m8[24m 
## [38;5;250m 6[39m AAPL   2009-06     0.050[4m1[24m 
## [38;5;250m 7[39m AAPL   2009-07     0.140  
## [38;5;250m 8[39m AAPL   2009-08     0.031[4m5[24m 
## [38;5;250m 9[39m AAPL   2009-09     0.098[4m9[24m 
## [38;5;250m10[39m AAPL   2009-10     0.021[4m1[24m 
## [38;5;246m# … with more rows[39m
```

```r
# 3.2 By Year ----
lags_diffs_query %>%
    
    mutate(year = substr(date, start = 1, stop = 4)) %>%
    
    group_by(symbol, year) %>%
    summarize(pct_change = sum(pct_diff_1, na.rm = TRUE)) %>%
    ungroup() 
```

```
## `summarise()` has grouped output by 'symbol'. You can override using the `.groups` argument.
```

```
## [38;5;246m# Source:   lazy query [?? x 3][39m
## [38;5;246m# Database: sqlite 3.34.1 [/home/dnn/Data_science/Git/Business
## #   science/Database-SQL/lab_22_sql_advanced - llpro/lab_22_sql_advanced - llpro/stocks.sqlite][39m
##    symbol year  pct_change
##    [3m[38;5;246m<chr>[39m[23m  [3m[38;5;246m<chr>[39m[23m      [3m[38;5;246m<dbl>[39m[23m
## [38;5;250m 1[39m AAPL   2009     0.899  
## [38;5;250m 2[39m AAPL   2010     0.462  
## [38;5;250m 3[39m AAPL   2011     0.262  
## [38;5;250m 4[39m AAPL   2012     0.325  
## [38;5;250m 5[39m AAPL   2013     0.119  
## [38;5;250m 6[39m AAPL   2014     0.365  
## [38;5;250m 7[39m AAPL   2015     0.005[4m0[24m[4m2[24m
## [38;5;250m 8[39m AAPL   2016     0.145  
## [38;5;250m 9[39m AAPL   2017     0.411  
## [38;5;250m10[39m AAPL   2018    -[31m0[39m[31m.[39m[31m0[39m[31m14[4m4[24m[39m 
## [38;5;246m# … with more rows[39m
```

```r
# 3.3 By Quarter ----
pct_change_qtr_query <- lags_diffs_query %>%
    
    mutate(year = substr(date, start = 1, stop = 4)) %>%
    mutate(mon  = substr(date, start = 6, stop = 7)) %>%
    
    mutate(qtr  = NA) %>%
    mutate(qtr  = ifelse(mon %in% c("01", "02", "03"), "Q1", qtr)) %>%
    mutate(qtr  = ifelse(mon %in% c("04", "05", "06"), "Q2", qtr)) %>%
    mutate(qtr  = ifelse(mon %in% c("07", "08", "09"), "Q3", qtr)) %>%
    mutate(qtr  = ifelse(mon %in% c("10", "11", "12"), "Q4", qtr)) %>%
    
    mutate(year_qtr = sql("year || '-' || qtr")) %>%
    
    group_by(symbol, year_qtr) %>%
    summarize(pct_change = sum(pct_diff_1, na.rm = TRUE)) %>%
    ungroup() 
    

pct_change_qtr_query %>% show_query()
```

```
## `summarise()` has grouped output by 'symbol'. You can override using the `.groups` argument.
```

```
## <SQL>
## SELECT `symbol`, `year_qtr`, SUM(`pct_diff_1`) AS `pct_change`
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `diff_1`, `pct_diff_1`, `growth`, `year`, `mon`, `qtr`, year || '-' || qtr AS `year_qtr`
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `diff_1`, `pct_diff_1`, `growth`, `year`, `mon`, CASE WHEN (`mon` IN ('10', '11', '12')) THEN ('Q4') WHEN NOT(`mon` IN ('10', '11', '12')) THEN (`qtr`) END AS `qtr`
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `diff_1`, `pct_diff_1`, `growth`, `year`, `mon`, CASE WHEN (`mon` IN ('07', '08', '09')) THEN ('Q3') WHEN NOT(`mon` IN ('07', '08', '09')) THEN (`qtr`) END AS `qtr`
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `diff_1`, `pct_diff_1`, `growth`, `year`, `mon`, CASE WHEN (`mon` IN ('04', '05', '06')) THEN ('Q2') WHEN NOT(`mon` IN ('04', '05', '06')) THEN (`qtr`) END AS `qtr`
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `diff_1`, `pct_diff_1`, `growth`, `year`, `mon`, CASE WHEN (`mon` IN ('01', '02', '03')) THEN ('Q1') WHEN NOT(`mon` IN ('01', '02', '03')) THEN (`qtr`) END AS `qtr`
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `diff_1`, `pct_diff_1`, `growth`, `year`, `mon`, NULL AS `qtr`
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `diff_1`, `pct_diff_1`, `growth`, `year`, SUBSTR(`date`, 6, 2) AS `mon`
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `diff_1`, `pct_diff_1`, `growth`, SUBSTR(`date`, 1, 4) AS `year`
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `diff_1`, `pct_diff_1`, SUM(`pct_diff_1`) OVER (PARTITION BY `symbol` ORDER BY `date` ROWS UNBOUNDED PRECEDING) + 1.0 AS `growth`
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `diff_1`, `diff_1` / `lag_1` AS `pct_diff_1`
## FROM (SELECT `symbol`, `date`, `adjusted`, `lag_1`, `adjusted` - `lag_1` AS `diff_1`
## FROM (SELECT `symbol`, `date`, `adjusted`, LAG(`adjusted`, 1, NULL) OVER (PARTITION BY `symbol` ORDER BY `date`) AS `lag_1`
## FROM (SELECT `symbol`, `date`, `adjusted`
## FROM `stock_history`)))))
## WHERE (NOT(((`lag_1`) IS NULL)))))))))))
## GROUP BY `symbol`, `year_qtr`
```

```r
# Plot percentage change
g <- pct_change_qtr_query %>%
    collect() %>%
    
    mutate(date = as.yearqtr(year_qtr, format = "%Y-Q%q") %>% as.Date()) %>%
    
    ggplot(aes(date, pct_change, color = symbol, group = symbol)) +
    geom_line() +
    facet_wrap(~ symbol, ncol = 1) +
    scale_color_tq() +
    theme_tq()
```

```
## `summarise()` has grouped output by 'symbol'. You can override using the `.groups` argument.
```

```r
ggplotly(g)
```

```
## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

```
## Error in path.expand(path): invalid 'path' argument
```

```r
# # Plot growth
# g <- pct_change_qtr_query %>% 
#     collect() %>%
#     
#     mutate(date = zoo::as.yearqtr(year_qtr, format = "%Y-Q%q") %>% lubridate::as.Date()) %>%
#     
#     group_by(symbol) %>%
#     arrange(year_qtr) %>%
#     mutate(growth = cumsum(pct_change) + 1) %>%
#     ungroup() %>%
#     
#     ggplot(aes(date, growth, color = symbol)) +
#     geom_line() +
#     theme_tq() +
#     scale_color_tq()
# 
# ggplotly(g)

# DISCONNECT ----
connections::connection_close(con_stocks)
```

