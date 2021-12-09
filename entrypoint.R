library(lubridate)
library(dplyr)
library(RPostgres)
library(DBI)
library(dbx)
library(pool)
library(plumber)
library(dbplyr)

# source("/Users/adam/Documents/SunnyD/sunnyday_postgres_keys.R")

# Connect to database
con <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("POSTGRESQL_DATABASE"),
  host = Sys.getenv("POSTGRESQL_HOSTNAME"),
  port = Sys.getenv("POSTGRESQL_PORT"),
  password = Sys.getenv("POSTGRESQL_PASSWORD"),
  user = Sys.getenv("POSTGRESQL_USER")
)

sensor_id_list <- con %>%
  tbl("sensor_locations") %>%
  pull(sensor_ID)

camera_id_list <- con %>%
  tbl("camera_locations") %>%
  pull(camera_ID)

api_keys <- con %>%
  tbl("api_keys") %>%
  pull("keys")

pr("plumber.R") %>% pr_run(host='0.0.0.0', port = 8000)

