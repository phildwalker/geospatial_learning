library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(eageo)
library(eatools)
library(rnoaa)

# get local weather stations ------------------------
con <- eadb::edw_connection('DS_Team')
stations_exists <- DBI::dbExistsTable(con, DBI::Id(schema = 'environment',
                                                table = 'noaa_weather_stations'))

local_counties <- c('Guilford', 'Alamance','Forsyth', 'Rockingham', 'Randolph')
local_fips <- fipscodes$fips[fipscodes$state=='North Carolina' &
                               fipscodes$county %in% local_counties]

rnoaa_token <- "TQOjcVnAkCVhILAPOEkyHlbcmgJUISzy"
Sys.setenv(NOAA_KEY = rnoaa_token)
#ncdc_datasets_avail <- ncdc_datasets()
#View(ncdc_datasets_avail$data)
#daily_summaries_id <- 'GHCND'


bb<- list()
bb[['l']] <- -80.518
bb[['t']] <- 36.543
bb[['r']] <- -79.236
bb[['b']] <- 35.505

#stations in our area with data in the last 2 weeks
stations_avail <- ncdc_stations(extent=c(bb[['b']],bb[['l']],
                                         bb[['t']],bb[['r']]),
                                startdate = format(Sys.Date() - 60,'%Y%m%d'),
                                limit = 1000)
library(sf)
stations_avail$data <- eageo::put_pts_in_polygon(stations_avail$data,
                                                 eageo::sf_nc_counties %>% select(County = NAME)
)


local_stations <- stations_avail$data[stations_avail$data$County %in%
                                        local_counties &
                                        grepl('^GHCND',stations_avail$data$id),] %>%
  select(id, name, County, datacoverage,
         mindate, maxdate, latitude, longitude, elevation,
         elevationUnit)

if(stations_exists){
  local_stations_db <- DBI::dbReadTable(con, DBI::Id(schema = 'environment',
                                                     table = 'noaa_weather_stations'))
notindb <- setdiff(local_stations$id, local_stations_db$station_id)
norecentweather <- setdiff(local_stations_db$id, local_stations$station_id)
if(length(notindb)>0)message('These stations are not in the database and have weather in the last 2 months:\n',
                             paste(notindb,collapse = ', '))
if(length(norecentweather)>0)message('These stations are in the database and but have NO weather in the last 2 months:\n',
                             paste(norecentweather,collapse = ', '))
}


# get historical daily weather ------------------------
con <- eadb::edw_connection('DS_Team')
daily_exists <- DBI::dbExistsTable(con, DBI::Id(schema = 'environment',
                                table = 'noaa_weather_daily'))

DBI::dbDisconnect(con)

if(!daily_exists){ #takes a while, api may stop accepting calls
  if((Sys.info()[["sysname"]]!="Windows")) stop('Full update necessary; should update on personal machine in case of API call issues')

  #want all data since 2014-10-01 (FY 15 - current)
  date_ranges <- data.frame(start = seq.Date(as.Date('2014-10-01'), Sys.Date(), '1 week'),
                            end = seq.Date(as.Date('2014-10-01'), Sys.Date(), '1 week') + 6)

  df_out <- data.frame()
  for(i in 287:nrow(date_ranges)){
    t1 <- date_ranges$start[i]
    t2 <- date_ranges$end[i]
    df_tmp <- ncdc(stationid = local_stations$id,
                   startdate = t1,
                   enddate = t2,
                   datasetid = "GHCND", limit = 1000,
                   add_units = T)
    if(nrow(df_tmp$data)==1000) {
      message('i = ',i,' - ',t1,'to',t2,' returned more than 1000 records; retrieving daily')
      # go daily
      daily_dates <- seq.Date(t1, t2, by = '1 day')
      for(j in daily_dates){
        df_tmp <- NULL
        df_tmp$data <- data.frame()
        df_tmp_daily <- ncdc(stationid = local_stations$id,
                             startdate = j,
                             enddate = j,
                             datasetid = "GHCND", limit = 1000,
                             add_units = T)
        df_tmp$data <- rbind(df_tmp$data, df_tmp_daily)
      }
    }
    df_out <- rbind(df_out, df_tmp$data)
    Sys.sleep(2) #don't piss off the API
    eatools::counter_verbose(i, nrow(date_ranges))
  }

  #one row per day per stations with datatype~value
  df_wide <- df_out %>%
    mutate(value = ifelse(grepl('tenths',units), #tenths to whole
                          value/10, value),
           value = ifelse(units=='celcius_tenths',
                          (value)*(9/5) + 32,#to fahrenheit
                          value)
    ) %>%
    select(date, station, datatype, value) %>%
    tidyr::pivot_wider(id_cols = c('date', 'station'),
                       names_from = c(datatype),
                       values_from = value) %>%
    rename('station_id' = 'station') %>%
    left_join(local_stations %>% select(station_id = id, station_name = name),
              by = 'station_id') %>%
    select(date, station_id, station_name, everything()) %>%
    mutate(date = as.Date(date))

  #write to environment.noaa_weather_daily
  con <- eadb::edw_connection('DS_Team')
  DBI::dbWriteTable(con,name = DBI::Id(schema = 'environment',
                                       table = 'noaa_weather_daily'),
                    value = df_wide %>% mutate(UpdateDTS = Sys.time())
  )
  DBI::dbDisconnect(con)



  df_units <- df_out %>% select(datatype, units) %>%
    mutate(units = ifelse(units=='celcius_tenths',
                          'fahrenheit',units),
           units = gsub("_tenths","",units)) %>%
    unique.data.frame()

  datatypes <- rnoaa::ncdc_datatypes(datasetid = 'GHCND', limit = 1000)

  #write to environment.noaa_weather_datatypes
  con <- eadb::edw_connection('DS_Team')
  DBI::dbWriteTable(con,name = DBI::Id(schema = 'environment',
                                       table = 'noaa_weather_datatypes'),
                    value = df_units %>% left_join(datatypes$data, by = c('datatype' = 'id')) %>%
                      transmute(datatype, description = name,
                                units, UpdateDTS = Sys.time())
  )
  DBI::dbDisconnect(con)
  n_before <- 0
} else {
  con <- eadb::edw_connection('DS_Team')
  df_wide <- DBI::dbReadTable(con, DBI::Id(schema = 'environment',
                                            table = 'noaa_weather_daily'))
  DBI::dbDisconnect(con)
  n_before <- nrow(df_wide)
}


# check for new/missing stations ------------------------
missing_stations <- setdiff(local_stations$id[grepl('^GHCND',local_stations$id)],
                            df_wide$station_id)
if(length(missing_stations)>0){
  message('Getting historical data for previously missing stations')
  date_ranges <- data.frame(start = seq.Date(as.Date('2014-10-01'), Sys.Date(), '1 week'),
                            end = seq.Date(as.Date('2014-10-01'), Sys.Date(), '1 week') + 6)
df_new_stations <- data.frame()
  for(i in 1:nrow(date_ranges)){
    t1 <- date_ranges$start[i]
    t2 <- date_ranges$end[i]
    df_tmp <- ncdc(stationid = missing_stations,
                   startdate = t1,
                   enddate = t2,
                   datasetid = "GHCND", limit = 1000,
                   add_units = T)
    if(nrow(df_tmp$data)==1000) {
      message('i = ',i,' - ',t1,'to',t2,' returned more than 1000 records; retrieving daily')
      # go daily
      daily_dates <- seq.Date(t1, t2, by = '1 day')
      for(j in daily_dates){
        df_tmp <- NULL
        df_tmp$data <- data.frame()
        df_tmp_daily <- ncdc(stationid = local_stations$id,
                             startdate = j,
                             enddate = j,
                             datasetid = "GHCND", limit = 1000,
                             add_units = T)
        df_tmp$data <- rbind(df_tmp$data, df_tmp_daily)
      }
    }
    df_new_stations <- rbind(df_new_stations, df_tmp$data)
    Sys.sleep(2) #don't piss off the API
    eatools::counter_verbose(i, nrow(date_ranges))
  }

}


# get all new daily weather since last update ------------------------
#last full day of data from:
con <- eadb::edw_connection('DS_Team')
last_avail <- DBI::dbGetQuery(con, "SELECT D.station_id, D.station_name,
                                   max(D.date) AS LastDate
                                   FROM [DS_Team].[environment].[noaa_weather_daily] D
                              INNER JOIN DS_Team.environment.noaa_weather_stations S ON D.station_id = S.station_id
                              WHERE dates_avail_perc > 0.5 --where it matters
                                   GROUP BY D.station_id, D.station_name")
(t1 <- min(last_avail$LastDate))

date_ranges_update <- data.frame(start = seq.Date(t1, Sys.Date(), '1 week'),
                                 end = seq.Date(t1, Sys.Date(), '1 week') + 6)

df_update <- data.frame()

for(i in 1:nrow(date_ranges_update)){
  t1 <- date_ranges_update$start[i]
  t2 <- date_ranges_update$end[i]
  df_tmp <- ncdc(stationid = local_stations$id,
                 startdate = t1,
                 enddate = t2,
                 datasetid = "GHCND", limit = 1000,
                 add_units = T)
  if(nrow(df_tmp$data)==1000) {
    message('i = ',i,' - ',t1,'to',t2,' returned more than 1000 records; retrieving daily')
    # go daily
    daily_dates <- seq.Date(t1, t2, by = '1 day')
    for(j in daily_dates){
      df_tmp <- NULL
      df_tmp$data <- data.frame()
      df_tmp_daily <- ncdc(stationid = local_stations$id,
                           startdate = j,
                           enddate = j,
                           datasetid = "GHCND", limit = 1000,
                           add_units = T)
      df_tmp$data <- rbind(df_tmp$data, df_tmp_daily)
    }
  }
  df_update <- rbind(df_update, df_tmp$data)
  Sys.sleep(2) #don't piss off the API
  eatools::counter_verbose(i, nrow(date_ranges_update))
}

df_update <- df_update %>%
  mutate(value = ifelse(grepl('tenths',units), #tenths to whole
                        value/10, value),
         value = ifelse(units=='celcius_tenths',
                        (value)*(9/5) + 32,#to fahrenheit
                        value)
  ) %>%
  select(date, station, datatype, value) %>%
  tidyr::pivot_wider(id_cols = c('date', 'station'),
                     names_from = c(datatype),
                     values_from = value) %>%
  rename('station_id' = 'station') %>%
  left_join(local_stations %>% select(station_id = id, station_name = name),
            by = 'station_id') %>%
  select(date, station_id, station_name, everything()) %>%
  mutate(date = as.Date(date))



# drop any duplicate daily weather from table ------------------------
con <- eadb::edw_connection('DS_Team')
rows_to_delete <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS row_count FROM DS_Team.environment.noaa_weather_daily WHERE date IN ('",
                            paste0(unique(df_update$date), collapse = "', '"),"')"))
if(nrow(df_update) == rows_to_delete$row_count){
  message('No new historical records available')
} else {
DBI::dbGetQuery(con, paste0("DELETE FROM DS_Team.environment.noaa_weather_daily WHERE date IN ('",
                            paste0(unique(df_update$date), collapse = "', '"),"')"))

# write updated daily weather to table -----------------------------
DBI::dbWriteTable(con, DBI::Id(schema = 'environment',
                               table = 'noaa_weather_daily'),
                  value = df_update %>% mutate(UpdateDTS = Sys.time()),
                  append = TRUE
)
  df_wide <- bind_rows(df_wide, df_update)
}
DBI::dbDisconnect(con)


# add any new stations found ----------------------------------------
if(length(missing_stations)>0){

  message('New stations identified.')

  missing_stations_written <- df_update %>% filter(station_id %in% missing_stations) %>%
    select(date) %>% unique()

  df_new_to_write <- df_new_stations %>%
    filter(!as.Date(date) %in% missing_stations_written$date) %>%
    mutate(value = ifelse(grepl('tenths',units), #tenths to whole
                          value/10, value),
           value = ifelse(units=='celcius_tenths',
                          (value)*(9/5) + 32,#to fahrenheit
                          value)
    ) %>%
    select(date, station, datatype, value) %>%
    tidyr::pivot_wider(id_cols = c('date', 'station'),
                       names_from = c(datatype),
                       values_from = value) %>%
    rename('station_id' = 'station') %>%
    left_join(local_stations %>% select(station_id = id, station_name = name),
              by = 'station_id') %>%
    select(date, station_id, station_name, everything()) %>%
    mutate(date = as.Date(date))



  con <- eadb::edw_connection('DS_Team')
  DBI::dbWriteTable(con,name = DBI::Id(schema = 'environment',
                                       table = 'noaa_weather_daily'),
                    value = df_new_to_write %>% mutate(UpdateDTS = Sys.time()),
                    append = TRUE
  )
  DBI::dbDisconnect(con)

  df_wide <- bind_rows(df_wide, df_new_to_write)
}

#update the data availability summary if new data present.
if(nrow(df_wide)!=n_before){

data_avail_summary <-
  df_wide %>% mutate(total_dates = n_distinct(date)) %>%
  group_by(station_id, station_name) %>%
  mutate(min_date = min(date),
         max_date = max(date),
         dates_avail_perc = n_distinct(date)/total_dates,
         row_count = n()) %>%
  tidyr::pivot_longer(cols = (names(df_wide)[4]):(rev(names(df_wide)[names(df_wide)!='UpdateDTS'])[1])) %>%
  filter(!is.na(value)) %>%
  group_by(min_date, max_date, dates_avail_perc, row_count,
           name, .add = T) %>%
  summarise(avail = n()) %>%
  mutate(avail = avail/row_count) %>%
  group_by(station_id, station_name, min_date, max_date,
           dates_avail_perc, row_count) %>%
  ungroup() %>% arrange(name) %>%
  tidyr::pivot_wider(names_from = name,
                     values_from = avail,values_fill = 0) %>%
  left_join(local_stations %>% select(station_id = id, County,
                                      latitude, longitude,
                                      api_reported_datacoverage = datacoverage),
            by = 'station_id') %>%
  select(station_id, station_name, County, latitude, longitude,
         api_reported_datacoverage, everything())

con <- eadb::edw_connection('DS_Team')
DBI::dbWriteTable(con, DBI::Id(schema = 'environment',
                               table = 'noaa_weather_stations'),
                  value = data_avail_summary %>% mutate(UpdateDTS = Sys.time()),
                  overwrite = T)
DBI::dbDisconnect(con)
message('Data availability summary update in stations table')
}
