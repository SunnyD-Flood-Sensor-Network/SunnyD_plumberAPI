#* Read water level for sites
#* @param key API keys
#* @param min_date Minimum date (mmddyyyy)
#* @param max_date Maximum date (mmddyyyy)
#* @param sensor_id Sensor ID (typically: site acronym, "_", number)
#* @get /read_water_level
function(key = "", min_date="", max_date="", sensor_id=""){
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

  if(!key %in% api_keys){
    stop("WRONG KEY!")
  }

  if(key %in% api_keys){
    con %>%
      tbl("sensor_data") %>%
      filter(sensor_ID %in% sensor_id,
             date >= min_date_parsed & date <= max_date_parsed) %>%
      collect()
  }
}

#* Write water level for sites
#* @param key API Key
#* @param place City name, State name of location
#* @param sensor_id Sensor ID
#* @param dttm Datetime of sample (mmddyyyyhhmmss)
#* @param level Water level relative to ground
#* @param voltage Voltage of sensor
#* @param notes Misc notes about sample
#* @post /write_water_level
function(key = "", place="", sensor_id="", dttm="", level="", voltage="", notes=""){
  date_parsed = lubridate::mdy_hms(dttm, tz = "EST")

  if(is.na(date_parsed)){
    stop("min_date OR max_date in wrong format. must be 'mdy'!")
  }

  if(!sensor_id %in% sensor_id_list){
    stop("sensor_id IS INVALID")
  }

  if(!key %in% api_keys){
    stop("WRONG KEY!")
  }

  if(key %in% api_keys){

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
