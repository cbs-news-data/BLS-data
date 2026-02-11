# Load libraries
# Load required packages (install if needed)
# install.packages(c("blsR", "tidyverse", "stringr", "xml2", "lubridate", "dotenv", "DatawRappr"))

library(blsR)
library(tidyverse)
library(stringr)
library(xml2)
library(lubridate)
library(DatawRappr)
library(dotenv)

# Set options
options(scipen = 999)

# Load environment variables
tryCatch(load_dot_env(), error = function(e) {})
bls_key <- Sys.getenv("BLS_API_KEY")
dw_api_key <- Sys.getenv("DW_API_KEY")

# --- Get data ---
total_nonfarm_employment <- get_series_table(
  "CES0000000001",
  start_year = 2022,
  end_year = 2026,
  registrationKey = bls_key
) %>%
  mutate(
    month = str_sub(period, -2),
    date = as.Date(paste0(year, "-", month, "-01"))
  ) %>%
  arrange(date) %>%
  select(label = date, value) %>%
  mutate(
    value = (value - lag(value)) * 1000  # Convert to monthly change in thousands
  ) %>%
  filter(label >= as.Date("2022-01-01")) %>%
  drop_na()

# Save to CSV
write.csv(total_nonfarm_employment, "data/monthly_change_nonfarm_employment.csv", row.names = FALSE)

# --- Push to Datawrapper ---
datawrapper_auth(api_key = dw_api_key)

max_date <- max(total_nonfarm_employment$label)
min_date <- min(total_nonfarm_employment$label)
max_date_pretty <- format(max_date, "%B %Y")

dw_data_to_chart(total_nonfarm_employment, "XlGvQ", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "XlGvQ",
  api_key = dw_api_key,
  annotate = paste("Data through", max_date_pretty, "<br>Note: Data is seasonally adjusted.")
)

dw_publish_chart(chart_id = "XlGvQ")

# --- Prepare XML Version ---
value_max <- max(total_nonfarm_employment$value)
value_min <- min(total_nonfarm_employment$value)

date_min_pretty <- format(min(total_nonfarm_employment$label), "%b %Y") %>% str_replace_all(" 0", " ")
date_max_pretty <- format(max(total_nonfarm_employment$label), "%b %Y") %>% str_replace_all(" 0", " ")

total_nonfarm_employment_forXML <- total_nonfarm_employment %>%
  mutate(
    showLabel = case_when(
      value == value_max ~ "1",
      value == value_min ~ "1",
      label == max_date ~ "1",
      TRUE ~ "0"
    ),
    showValue = showLabel,
    label = format(label, "%b %Y") %>% str_replace_all(" 0", " "),
    valueToShow = paste0(round(value / 1000), "K")
  )

# Build XML
employment_chart <- xml_new_root("chart")
xml_add_child(employment_chart, "title", "Monthly changes in employment")
xml_add_child(employment_chart, "subtitle", "Nonfarm payroll employment, seasonally adjusted")
xml_add_child(employment_chart, "type", "line")
xml_add_child(employment_chart, "x-axis", paste0(date_min_pretty, "|", date_max_pretty))
xml_add_child(employment_chart, "y-axis", "100K|200K|300K")
xml_add_child(employment_chart, "y-max", 300000)

# Add data rows
walk(1:nrow(total_nonfarm_employment_forXML), function(i) {
  row_node <- xml_add_child(employment_chart, "dataPoint")
  walk(names(total_nonfarm_employment_forXML), function(col) {
    xml_add_child(row_node, col, as.character(total_nonfarm_employment_forXML[[i, col]]))
  })
})

# Add metadata
xml_add_child(employment_chart, "source", "Bureau of Labor Statistics")
xml_add_child(employment_chart, "date", paste0("As of ", date_max_pretty))
xml_add_child(employment_chart, "qualifier", " ")

# Write to file
write_xml(employment_chart, "data/monthly_employment_change.xml")