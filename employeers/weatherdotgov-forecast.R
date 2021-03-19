# get weather forecast for ed locations -----------------------
library(eageo)
library(rnoaa)
library(eatools)
library(dplyr)

try_function <- function(FUN, max_attempts = 5, ...){
  r <- NULL
  attempt <- 0
  while( is.null(r) && attempt <= max_attempts ) {
    attempt <- attempt + 1
    try(
      r <- FUN(...)
    )
  }
  return(r)
}

con <- eadb::edw_connection('DS_Team')
forecasts_exist <- DBI::dbExistsTable(conn = con,
                                      name = DBI::Id(schema = 'environment',
                                                     table = 'ed_weather_forecasts'))
if(forecasts_exist){
  last_update <- DBI::dbGetQuery(con, "SELECT MAX(date_forecast_generated) AS max_date
                                 FROM DS_Team.environment.ed_weather_forecasts")
  last_update <- last_update$max_date
} else last_update <- as.Date('1900-01-01')

if(last_update < Sys.Date()){
ed_loc <- eageo::ed_urgent_locations
ed_loc <- eadb::submit_edw_query_with_df(ed_loc,
                                         "SET NOCOUNT ON;
                                         SELECT D.DepartmentID, E.*
                                         FROM ed_loc E
                                         LEFT JOIN Epic.Reference.Department D
                                         ON E.department_dsc = D.DepartmentNM",
                                         show_query = F)


rnoaa_token <- "TQOjcVnAkCVhILAPOEkyHlbcmgJUISzy"
Sys.setenv(NOAA_KEY = rnoaa_token)
out <- list()
for(i in 1:nrow(ed_loc)){

  dat <- try_function(get_weather_forecast, 5, #try 5 times
                      ed_loc$lat[i], ed_loc$lon[i])
  dat$DepartmentNM <- ed_loc$department_dsc[i]
  dat$DepartmentID <- ed_loc$DepartmentID[i]
  out[[i]] <- dat
  Sys.sleep(5) #for API calls
}

# clean data -----------------------
out2 <- do.call(rbind, out)

out2 <- out2 %>%
  select(-forecastInterval) %>%
  mutate(forecast_date = Sys.Date()) %>%
  select(forecast_date,
         DepartmentID, DepartmentNM,
         everything())

# manipulate to daily forecast -----------------------
out_daily <- out2 %>%
  mutate(date = as.Date(endTime),
         date_forecast_generated = forecast_date) %>%
  group_by(DepartmentID, DepartmentNM,
           date_forecast_generated, date) %>%
  summarise(min_temp = min(temperature, na.rm = T),
            max_temp = max(temperature, na.rm = T),
            min_wind = min(windSpeedMin, na.rm = T),
            max_wind = max(windSpeedMax, na.rm = T),
            precip_prob = max(precip_prob, na.rm = T),
            precip_amt = sum(precip_amt, na.rm = T)) %>%
  filter(date > Sys.Date())

# write to DS_Team ------------------------

DBI::dbWriteTable(con, DBI::Id(schema = 'environment',
                               table = 'ed_weather_forecasts'),
                  value = out_daily %>% mutate(UpdateDTS = Sys.time()),
                  append = forecasts_exist)
DBI::dbDisconnect(con)
} else{
  cat('\nForecasts already updated today.\n')
}
#alrts <- get_local_weather_alerts()
