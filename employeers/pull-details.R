# Purpose: Loop through NCAIS codes to pull down all NC Industries with
#    categorizations.
# Author: Michael DeWitt
# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------

library(rvest)
library(tidyverse)
lapply(fs::dir_ls(here::here("R")), source)

# source url --------------------------------------------------------------

url <- "https://accessnc.nccommerce.com/business/business_custom_search_infogroup.html"


industries <- readr::read_csv(here::here("data",
                                         "industry_codes.csv"))

safe_pull <- purrr::safely(pull_industry_information)

# run through industry codes ----------------------------------------------

collector <- list()
code <- pull(industries, industry_code)
industry_desc <- pull(industries, industry_desc)

for(i in 1:nrow(industries)){
#for(i in 1:5){

  collector[[i]] <- safe_pull(url = url,
                                              industry_code = code[i],
                                              id = industry_desc[i])
  Sys.sleep(2)
}

test <- pull_industry_information(url = url, industry_code = "4543", id = "1")

# combine -----------------------------------------------------------------

output <- collector %>%
  map("result")

output_2 <- do.call(rbind, output)

output_clean <- dplyr::select(output_2, -x)

data.table::fwrite(output_clean, here::here("data", "output.csv"))
# write to ds team --------------------------------------------------------


connection_name <- DBI::dbConnect(odbc::odbc(), dsn = "DS_Team")

## old location ----
# DBI::dbWriteTable(connection_name,
#                   name = "tbl_employer_industry_crosswalk",
#                   value = output_clean)

# new location -----
DBI::dbWriteTable(connection_name,
                  name = "DS_Team.localplaces.nc_employers",
                  value = output_clean)

DBI::dbDisconnect(connection_name)
