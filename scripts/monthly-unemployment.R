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
civilian_unemployment_rate <- get_series_table(
  'LNS14000000', 
  start_year = 2022, 
  end_year = 2025, 
  registrationKey = bls_key
) %>%
  mutate(
    month = str_sub(period, start = -2),
    year = as.character(year),
    date = as.Date(paste0(year, "-", month, "-01"))
  ) %>%
  arrange(date) %>%
  select(label = date, value) %>% 
  mutate(value = str_replace_all(value, "-", ""))

# Save CSV
write.csv(civilian_unemployment_rate, "data/unemployment_rate.csv", row.names = FALSE)

# --- Push to Datawrapper ---
max_date <- max(civilian_unemployment_rate$label)
max_date_pretty <- format(max_date, "%B %Y")

datawrapper_auth(api_key = dw_api_key)

dw_data_to_chart(civilian_unemployment_rate, "tZqDq", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "tZqDq",
  api_key = dw_api_key,
  annotate = paste("Data through", max_date_pretty, "<br>Note: Data is seasonally adjusted.")
)

dw_publish_chart(chart_id = "tZqDq")

# --- Prepare XML Version ---
date_min <- min(civilian_unemployment_rate$label)
date_max <- max(civilian_unemployment_rate$label)
#value_min <- min(civilian_unemployment_rate$value)
value_min <- civilian_unemployment_rate %>% 
  filter(value != "") %>% 
  summarise(min_value = min(value, na.rm = TRUE)) %>% 
  pull(min_value)
value_max <- max(civilian_unemployment_rate$value)
value_max_rounded <- round(as.numeric(value_max), digits = 0)
value_min_rounded <- round(as.numeric(value_min), digits = 0)

# Generate 4 to 5 axis ticks using pretty()
ticks <- pretty(c(0, value_max), n = 4)
#get max ticks value
ticks_max <- max(ticks)
# Convert to character string with "|" separator
ticks_string <- paste0(ticks, "%", collapse = "|")
ticks_string <- str_replace(ticks_string, "0%", "")
ticks_string <- str_replace(ticks_string, "\\|", "")


# Format axis labels
date_min_pretty <- str_replace_all(format(date_min, "%b %Y"), " 0", " ")
date_max_pretty <- str_replace_all(format(date_max, "%b %Y"), " 0", " ")
x_labels <- paste0(date_min_pretty, "|", date_max_pretty)

# Format data for XML
civilian_unemployment_rate_forXML <- civilian_unemployment_rate %>%
  mutate(
    showLabel = if_else(label == date_max | value %in% c(value_min, value_max), "1", "0"),
    showValue = showLabel,
    label = str_replace_all(format(label, "%b %Y"), " 0", " "),
    valueToShow = paste0(value, "%")
  )

# -----------------------------
# Build XML chart structure
# -----------------------------
xml_title <- "Monthly unemployment rate"
xml_subtitle <- "Civilians 16 years and older, seasonally adjusted"
xml_yaxis <- "3%|6%|9%|12%|15%"
xml_ymax <- 15
xml_source <- "Bureau of Labor Statistics"
xml_date <- paste0("As of ", date_max_pretty)
xml_type <- "line"
xml_qualifier <- " "

# Create chart node
unemployment_chart <- xml_new_root("chart")
xml_add_child(unemployment_chart, "title", xml_title)
xml_add_child(unemployment_chart, "subtitle", xml_subtitle)
xml_add_child(unemployment_chart, "type", xml_type)
xml_add_child(unemployment_chart, "x-axis", x_labels)
xml_add_child(unemployment_chart, "y-axis", xml_yaxis)
xml_add_child(unemployment_chart, "y-max", as.character(xml_ymax))

# Add data points
for (i in seq_len(nrow(civilian_unemployment_rate_forXML))) {
  row_node <- xml_add_child(unemployment_chart, "dataPoint")
  for (col_name in names(civilian_unemployment_rate_forXML)) {
    xml_add_child(row_node, col_name, as.character(civilian_unemployment_rate_forXML[[i, col_name]]))
  }
}

# Add metadata
xml_add_child(unemployment_chart, "source", xml_source)
xml_add_child(unemployment_chart, "date", xml_date)
xml_add_child(unemployment_chart, "qualifier", xml_qualifier)

# Save to XML file
write_xml(unemployment_chart, "data/unemployment_rate.xml")