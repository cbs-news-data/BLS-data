# Load required packages (install if needed)
# install.packages(c("blsR", "tidyverse", "stringr", "xml2", "lubridate", "dotenv", "DatawRappr"))

library(blsR)
library(tidyverse)
library(stringr)
library(xml2)
library(lubridate)
library(dotenv)
library(DatawRappr)

options(scipen = 999)

# Load environment variables
tryCatch(load_dot_env(), error = function(e) {}) 
bls_key <- Sys.getenv("BLS_API_KEY")
dw_api_key <- Sys.getenv("DW_API_KEY")

# --- Get data ---
# https://download.bls.gov/pub/time.series/ce/ce.industry
series_list <- list(
  CES9000000001 = "Government",
  CES2000000001 = "Construction",
  CES1000000001 = "Mining and logging",
  CES6000000001 = "Professional and business services",
  CES7000000001 = "Leisure and hospitality",
  CES8000000001 = "Other services",
  CES5500000001 = "Financial activities",
  CES6500000001 = "Private education and health services",
  CES5000000001 = "Information",
  CES4200000001 = "Retail trade",
  CES4142000001 = "Wholesale trade",
  CES4422000001 = "Utilities",
  CES4300000001 = "Transportation and warehousing"
  
)

# Fetch, calculate net change
get_latest_net_change <- function(series_id, sector_name) {
  df <- get_series_table(
    series_id,
    start_year = 2024,
    end_year = 2025,
    registrationKey = bls_key
  ) %>%
    mutate(
      month = str_sub(period, -2),
      date = as.Date(paste0(year, "-", month, "-01"))
    ) %>%
    arrange(desc(date)) %>%
    slice(1:2) %>%
    arrange(date)
  
  if (nrow(df) < 2) return(NULL)
  
  change <- df$value[2] - df$value[1]
  tibble(
    sector = sector_name,
    previous_month = df$date[1],
    latest_month = df$date[2],
    net_change = round(change * 1000),  # Convert from thousands to full number
    latest_value = df$value[2] * 1000   # Also multiply this if needed
  )
}

# Apply function to all series
net_changes <- map2_dfr(names(series_list), series_list, get_latest_net_change)

# Save to CSV
write_csv(net_changes, "data/net_changes_by_sector.csv")

# --- Push to Datawrapper ---
max_date <- max(net_changes$latest_month)
max_date_pretty <- format(max_date, "%B %Y")

datawrapper_auth(api_key = dw_api_key)

dw_data_to_chart(net_changes, "MPv9f", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "MPv9f",
  api_key = dw_api_key,
  annotate = paste("Data as of", max_date_pretty, "<br>Note: Data is seasonally adjusted.")
)

dw_publish_chart(chart_id = "MPv9f")