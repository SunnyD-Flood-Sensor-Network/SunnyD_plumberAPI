#* @apiTitle SunnyD Flooding API
#* @apiDescription This API contains functions for the SunnyD Flooding project, which measures and models urban coastal flooding
#* @apiContact list(name = "API Support", email = "gold@unc.edu")
#* @apiVersion 1.0.1

#------------------ Read water level ----------------
#* Read water level for sites
#* @param key API keys
#* @param min_date Minimum date (yyyymmdd)
#* @param max_date Maximum date (yyyymmdd)
#* @param sensor_id Sensor ID (typically: site acronym, "_", two-digit number)
#* @get /read_water_level
function(key, min_date, max_date, sensor_id){
  min_date_parsed = lubridate::ymd(min_date)
  max_date_parsed = lubridate::ymd(max_date)

  if(min_date_parsed > max_date_parsed){
    stop("min_date LARGER THAN max_date")
  }

  if(is.na(min_date_parsed) | is.na(max_date_parsed)){
    stop("min_date OR max_date in wrong format. must be 'yyyymmddd'!")
  }

  if(!sensor_id %in% sensor_id_list){
    stop("sensor_id IS INVALID")
  }

  if(!key %in% api_keys){
    stop("WRONG KEY!")
  }

  if(key %in% api_keys){
    return(con %>%
      tbl("sensor_data") %>%
      filter(sensor_ID %in% sensor_id,
             date >= min_date_parsed & date <= max_date_parsed) %>%
      collect())
  }
  rm(min_date_parsed)
  rm(max_date_parsed)
}

#------------------ Read last water level ----------------
#* Read last water level for sites
#* @param key API keys
#* @param sensor_id Sensor ID (typically: site acronym, "_", two-digit number)
#* @get /latest_water_level
function(key, sensor_id){
  if(!sensor_id %in% sensor_id_list){
    stop("sensor_id IS INVALID")
  }

  if(!key %in% api_keys){
    stop("WRONG KEY!")
  }

  if(key %in% api_keys){
    con %>%
      tbl("sensor_data") %>%
      filter(sensor_ID %in% sensor_id) %>%
      filter(date == max(date, na.rm=T)) %>%
      collect()
  }
}

#-------------- Write water level -----------------
#* Write water level for sites
#* @param key API Key
#* @param place City name, State name of location
#* @param sensor_id Sensor ID
#* @param dttm Datetime of sample (yyyymmddhhmmss)
#* @param pressure Water pressure
#* @param wtemp Water Temperature
#* @param voltage Voltage of sensor
#* @param notes Misc notes about sample
#* @post /write_water_level
function(key, place, sensor_id, dttm, timezone = "EST",pressure, wtemp, voltage="", notes="", seqNum = "", aX = "", aY = "", aZ = ""){
  date_parsed = lubridate::with_tz(lubridate::ymd_hms(dttm, tz = timezone), tzone = "UTC")

  if(is.na(date_parsed)){
    stop("min_date OR max_date in wrong format. must be 'yyyymmddhhmmss'!")
  }

  if(!sensor_id %in% sensor_id_list){
    stop("sensor_id IS INVALID")
  }

  if(!key %in% api_keys){
    stop("WRONG KEY!")
  }

  if(key %in% api_keys){

    dbx::dbxUpsert(conn = con,
               table = "sensor_data",
               records = tibble::tibble(
                    "place" = place,
                    "sensor_ID" = sensor_id,
                    "date" = date_parsed,
                    "pressure" = pressure,
                    "wtemp" = wtemp,
                    "voltage" = voltage,
                    "notes" = notes,
                    "seqNum" = seqNum,
                    "aX" = aX,
                    "aY" = aY,
                    "aZ" = aZ,
                    "processed" = F
                  ),
               where_cols = c("place","sensor_ID","date"),
               skip_existing = T)


    rm(date_parsed)
    return("SUCCESS!")
  }
}


#----------------------- Get sensor locations and latest data ------------------
#* @get /get_all_latest_data
#* @param key API key
#* Get latest data and sensor locations for all sites. Mainly for website display
function(key) {
  if (key %in% api_keys) {
    return(con %>%
      tbl("sensor_locations") %>%
      collect() %>%
      left_join(con %>%
                  tbl("sensor_data") %>%
                  group_by(sensor_ID) %>%
                  filter(date == max(date, na.rm = T)),
                copy = T))
  }
}
