# Purpose: Clean up raw NCAIS codes and write out a nice csv
#    this will be used in a loop to pull down the data
# Author: Michael
# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------

# -------------------------------------------------------------------------

in_file <- readLines(here::here("data-raw", "ncais-codes.txt"))


# process -----------------------------------------------------------------
in_file <- gsub("\\s\\s+","",in_file)

nice_descriptions <- gsub(".*=\"(.+)\".*", "\\1",in_file)

strip_html <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
}

# Combine the files together
out_file <- lapply(in_file ,strip_html)

out_file <- do.call(rbind, out_file)

out_file <- data.frame(industry_desc = nice_descriptions,
                       industry_code = out_file, stringsAsFactors = FALSE)

# write output ------------------------------------------------------------
data.table::fwrite(out_file,
                   here::here("data", "industry_codes.csv"))
