library(lubridate)
library(dplyr)
library(RPostgres)
library(DBI)
library(pool)
library(plumber)
library(dbplyr)

# Connect to database
con <- dbPool(
  drv =RPostgres::Postgres(),
  dbname = Sys.getenv("POSTGRESQL_DATABASE"),
  host = Sys.getenv("POSTGRESQL_HOSTNAME"),
  port = Sys.getenv("POSTGRESQL_PORT"),
  password = Sys.getenv("POSTGRESQL_PASSWORD"),
  user = Sys.getenv("POSTGRESQL_USER")
)

sensor_id_list <- con %>%
  tbl("sensor_locations") %>%
  pull(sensor_ID)

api_tokens <- c("bft_node1_EliqHXP!")

pr("plumber.R") %>% pr_run(host='0.0.0.0', port = 8000)