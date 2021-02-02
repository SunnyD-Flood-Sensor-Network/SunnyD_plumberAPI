library(lubridate)
library(dplyr)
library(RPostgres)
library(DBI)
library(pool)
library(plumber)
library(dbplyr)
library(magick)
library(aws.s3)
library(googledrive)

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

api_keys <- con %>%
  tbl("api_keys") %>%
  pull("keys")

parser_jpeg <- function(...) {
  parser_read_file(function(tmpfile) {
    magick::image_read(tmpfile, ...)
  })
}
register_parser("jpeg", parser_jpeg, fixed = c("image/jpeg", "image/jpg"))


pr("plumber.R") %>% pr_run(host='0.0.0.0', port = 8000)

