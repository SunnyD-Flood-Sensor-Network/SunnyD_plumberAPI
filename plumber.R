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
function(key, place, sensor_id, dttm, timezone = "EST",pressure, wtemp, voltage="4.0", notes="", seqNum = "1", aX = "1", aY = "1", aZ = "1"){
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

#------------------------ recalculate data ---------------------
#* @post /reanalyze_data
#* @param key API key
#* @param place ex: Beaufort, North Carolina
#* @param sensor_id ex: BF_02
#* @param start_time format: yyyymmddhhmmss
#* @param end_time format: yyyymmddhhmmss
#* @param time_zone default:America/New_York (EST/EDT)
#* This function tags the raw data in the provided time span so it is re-analyzed by our monitoring script that runs every 6 minutes. The monitoring script updates processed data table with the re-analyzed data.
function(key, place, sensor_id, start_time, end_time, time_zone = "America/New_York"){
  if(!key %in% api_keys){
    stop("WRONG KEY!")
  }

  if (key %in% api_keys) {
    parsed_start_date <- lubridate::ymd_hms(start_time, tz = time_zone)
    parsed_end_date <- lubridate::ymd_hms(end_time, tz = time_zone)

    data_to_analyze <- con %>%
      tbl("sensor_data") %>%
      filter(place == !!place,
             sensor_ID == sensor_id) %>%
      filter(date >= parsed_start_date & date < parsed_end_date) %>%
      collect() %>%
      mutate(processed = F)

    dbx::dbxUpdate(conn = con,
                   table="sensor_data",
                   records = data_to_analyze,
                   where_cols = c("place", "sensor_ID", "date")
    )

    rm(data_to_analyze)

    return("SUCCESS!")
  }
}

#------------------- Edit sensor location functions ------------------------
#* @get /get_sensor_info
#* @param key API key
#* @param place ex: Beaufort, North Carolina
#* @param sensor_id ex: BF_02
#* Get info for sensor locations. To return all sensor, leave 'place' and 'sensor_id' blank. To return all sensors for a 'place', leave just 'sensor_id' blank.
function(key, place = NA, sensor_id = NA){
  if(!key %in% api_keys){
    stop("WRONG KEY!")
  }

  if (key %in% api_keys) {
      if(is.na(place) & is.na(sensor_id)){
        return(
          con %>%
            tbl("sensor_locations") %>%
            collect()
        )
      }

    if(is.na(sensor_id) & !is.na(place)){
      return(
        con %>%
          tbl("sensor_locations") %>%
          filter(place == !!place) %>%
          collect()
      )
    }

    if(!is.na(place) & !is.na(sensor_id)){
      return(
        con %>%
          tbl("sensor_locations") %>%
          filter(place == !!place & sensor_id == sensor_ID) %>%
          collect()
      )
    }
  }
}

#* @post /create_sensor
#* @param key API key
#* @param place ex: Beaufort, North Carolina
#* @param sensor_id ex: BF_02
#* @param lng
#* @param lat
#* @param sensor_elevation in ft NAVD88
#* @param road_elevation in ft NAVD88
#* @param alert_offset Depth below road_elevation to trigger flood alerts. Default is 0.5 ft below road
#* @param notes
#* @param date_surveyed Date of last survey in YYYYMMDDHHMMSS. Just put in the date and use zeros for hms. Ex: 20211210000000 for Dec 10th, 2021.
#* Create a new sensor location
function(key, place, sensor_id, lng, lat, sensor_elevation, road_elevation, alert_offset = 0.5, notes = "", date_surveyed){
  if(!key %in% api_keys){
    stop("WRONG KEY!")
  }
  if (key %in% api_keys) {
    site_info <- tibble::tibble(
      "place" = place,
      "sensor_ID" = sensor_id,
      "lng" = as.numeric(lng),
      "lat" = as.numeric(lat),
      "sensor_elevation" = as.numeric(sensor_elevation),
      "road_elevation" = as.numeric(road_elevation),
      "alert_threshold" = as.numeric(road_elevation) - as.numeric(alert_offset),
      "notes" = notes,
      "date_surveyed" = lubridate::ymd_hms(date_surveyed)
    )

    dbx::dbxUpsert(conn = con,
                   table = "sensor_locations",
                   records = site_info,
                   where_cols = c("place","sensor_ID"),
                   skip_existing = T)

    rm(site_info)

    return(paste("SUCCESS!", "Created new sensor: ",sensor_id))

  }
}

#* @post /edit_sensor
#* @param key API key
#* @param place ex: Beaufort, North Carolina
#* @param sensor_id ex: BF_02
#* @param lng
#* @param lat
#* @param sensor_elevation
#* @param road_elevation
#* @param alert_offset
#* @param notes
#* @param date_surveyed Date of last survey in YYYYMMDDHHMMSS. Just put in the date and use zeros for hms. Ex: 20211210000000 for Dec 10th, 2021.
#* Create a new sensor location
#* Edit a sensor location. Fill in only the attributes you wish to change
function(key, place, sensor_id, lng = NA, lat = NA, sensor_elevation = NA, road_elevation = NA, alert_offset = NA, notes = NA, date_surveyed = NA){
  if(!key %in% api_keys){
    stop("WRONG KEY!")
  }
  if (key %in% api_keys) {
    new_info <- tibble::tibble(
      "place" = place,
      "sensor_ID" = sensor_id,
      "lng" = as.numeric(lng),
      "lat" = as.numeric(lat),
      "sensor_elevation" = as.numeric(sensor_elevation),
      "road_elevation" = as.numeric(road_elevation),
      "notes" = notes
    ) %>%
      mutate(alert_threshold = road_elevation - as.numeric(alert_offset)) %>%
      select_if(~any(!is.na(.)))

    changed_cols <-  colnames(new_info)[!colnames(new_info) %in% c("place","sensor_ID")]

    dbx::dbxUpsert(conn = con,
                   table = "sensor_locations",
                   records = new_info,
                   where_cols = c("place","sensor_ID"),
                   skip_existing = F)
    rm(new_info)

    return(paste("SUCCESS!", "Edited",sensor_id,"column(s):", changed_cols))

  }
}

# #* @post /delete_sensor
# #* @param key API key
# #* @param place ex: Beaufort, North Carolina
# #* @param sensor_id ex: BF_02
# #* Delete a sensor location. One at a time.
# function(key, place = "Beaufort, North Carolina", sensor_id){
#   if (key %in% api_keys) {
#     sensor_to_delete <- con %>%
#       tbl("sensor_locations") %>%
#       filter(place == !!place & sensor_id == sensor_ID) %>%
#       collect()
#
#     dbx::dbxDelete(conn = con,
#                    table = "sensor_locations",
#                    where = sensor_to_delete)
#     return(paste("SUCCESS!", sensor_id, "deleted"))
#   }
# }
