#install.packages("RSQLite")
library(DBI)
library(RSQLite)

library(tidyverse)
library(dbplyr)

?SQLite

# Connect to non-existant database to instantiate
con <- dbConnect(drv = SQLite(), dbname = "home_loan_applications")

# Read Files to add as Tables
applications_train_tbl   <- read_csv("../loan_repayment/home-credit-default-risk/application_train.csv")
applications_test_tbl    <- read_csv("../loan_repayment/home-credit-default-risk/application_test.csv")
bureau_tbl               <- read_csv("../loan_repayment/home-credit-default-risk/bureau.csv")
bureau_balance_tbl       <- read_csv("../loan_repayment/home-credit-default-risk/bureau_balance.csv")
previous_application_tbl <- read_csv("../loan_repayment/home-credit-default-risk/previous_application.csv")
pos_cash_balance_tbl     <- read_csv("../loan_repayment/home-credit-default-risk/POS_CASH_balance.csv")
credit_card_balance_tbl  <- read_csv("../loan_repayment/home-credit-default-risk/credit_card_balance.csv")
installment_payments_tbl <- read_csv("../loan_repayment/home-credit-default-risk/installments_payments.csv")

# Make Tables
DBI::dbWriteTable(conn = con, "applications_train", applications_train_tbl, overwrite = TRUE)
DBI::dbWriteTable(conn = con, "applications_test", applications_test_tbl, overwrite = TRUE)
DBI::dbWriteTable(conn = con, "bureau", bureau_tbl, overwrite = TRUE)
DBI::dbWriteTable(conn = con, "bureau_balance", bureau_balance_tbl, overwrite = TRUE)
DBI::dbWriteTable(conn = con, "previous_application", previous_application_tbl)
DBI::dbWriteTable(conn = con, "POS_CASH_balance", pos_cash_balance_tbl)
DBI::dbWriteTable(conn = con, "credit_card_balance", credit_card_balance_tbl)
DBI::dbWriteTable(conn = con, "installment_payments", installment_payments_tbl)


# Verify Tables were created
DBI::dbListTables(con)

tbl(con, "applications_train")


DBI::dbListTables(con) %>%
    map(.f = function(x) tbl(con, x)) %>%
    set_names(dbListTables(con))


dbDisconnect(con)
