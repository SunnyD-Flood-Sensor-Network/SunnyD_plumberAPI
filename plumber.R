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
  port = "5432",
  password = Sys.getenv("POSTGRESQL_PASSWORD"),
  user = Sys.getenv("POSTGRESQL_USER")
)

sensor_id_list <- con %>%
  tbl("sensor_locations") %>%
  pull(sensor_ID)

api_tokens <- c("bft_node1_EliqHXP!")

#* Read water level for sites
#* @param token Auth token
#* @get /read_water_level
function(token = "", min_date="", max_date="", sensor_id=""){
  min_date_parsed = lubridate::mdy(min_date)
  max_date_parsed = lubridate::mdy(max_date)

  if(min_date_parsed > max_date_parsed){
    stop("min_date LARGER THAN max_date")
  }

  if(is.na(min_date_parsed) | is.na(max_date_parsed)){
    stop("min_date OR max_date in wrong format. must be 'mdy'!")
  }

  if(!sensor_id %in% sensor_id_list){
    stop("sensor_id IS INVALID")
  }

  if(!token %in% api_tokens){
    stop("WRONG TOKEN!")
  }

  if(token %in% api_tokens){
    con %>%
      tbl("sensor_data") %>%
      filter(sensor_ID %in% sensor_id,
             date >= min_date_parsed & date <= max_date_parsed) %>%
      collect()
  }
}

#* Write water level for sites
#* @param token Auth token
#* @post /write_water_level
function(token = "", place="", sensor_id="", dttm="", level="", voltage="", notes=""){
  date_parsed = lubridate::mdy_hms(dttm, tz = "EST")

  if(is.na(date_parsed)){
    stop("min_date OR max_date in wrong format. must be 'mdy'!")
  }

  if(!sensor_id %in% sensor_id_list){
    stop("sensor_id IS INVALID")
  }

  if(!token %in% api_tokens){
    stop("WRONG TOKEN!")
  }

  if(token %in% api_tokens){

    dbAppendTable(conn = con,
                  name = "sensor_data",
                  value = tibble(
                    "place" = place,
                    "sensor_ID" = sensor_id,
                    "date" = date_parsed,
                    "level" = level,
                    "voltage" = voltage,
                    "notes" = notes
                  )
    )
  }
}
