#install pkgs if needed
# install.packages("blsR")
# install.packages("tidyverse")
# install.packages("stringr")
# install.packages("xml2")
#install.packages("lubridate")

library(blsR)
library(tidyverse)
library(stringr)
library(xml2)
library(lubridate)

options(scipen = 999)

# LNS14000000 Civilian unemployment rate

civilian_unemployment_rate <- get_series_table('LNS14000000', start_year = 2019, end_year = 2025) %>% 
  mutate(month = str_sub(period, start=-2)) %>% 
  mutate(year = as.character(year)) %>% 
  mutate(month = as.character(month)) %>% 
  mutate(date = paste0(year, "-", month, "-01")) %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(date) %>%
  select(date, value) %>% 
  rename(label = date)

write.csv(civilian_unemployment_rate, "data/unemployment_rate.csv", row.names = FALSE)

#get min and max labels
date_max <- max(civilian_unemployment_rate$label)
date_min <- min(civilian_unemployment_rate$label)
value_max <- max(civilian_unemployment_rate$value)
value_min <- min(civilian_unemployment_rate$value)

#get pretty min date
date_min_pretty <- format(min(as.Date(civilian_unemployment_rate$label)), "%b %Y")
date_min_pretty <-str_replace_all(date_min_pretty, " 0", " ")
#get pretty max date
date_max_pretty <- format(max(as.Date(civilian_unemployment_rate$label)), "%b %Y")
date_max_pretty <-str_replace_all(date_max_pretty, " 0", " ")

#simplify to "label" and "value" column + "showLabel" + showValue" + "valueToShow" column...change columns 
civilian_unemployment_rate_forXML <- civilian_unemployment_rate %>% 
  mutate(showLabel = case_when(value == value_max ~ "1",
                               value == value_min ~ "1",
                               label == date_max ~ "1",
                               TRUE ~ "0")) %>% 
  mutate(showValue = case_when(value == value_max ~ "1",
                               value == value_min ~ "1",
                               label == date_max ~ "1",
                               TRUE ~ "0")) %>% 
  mutate(label = format(as.Date(label), "%b %Y")) %>% 
  mutate(label = str_replace_all(label, " 0", " ")) %>% 
  mutate(valueToShow = paste0(value, "%"))


#get labels for x axis
labels = paste0(date_min_pretty, "|", date_max_pretty)

#convert US data to XML

#variables 
xml_title <- "Monthly unemployment rate"
xml_subtitle <- "Civilians 16 years and older, seasonally adjusted"
xml_xaxis <- labels #labels/values for x axis
xml_yaxis <- "3%|6%|9%|12%|15%" #labels/values for y axis, only fill out in necessary
xml_ymax <-  15 #float value for max value OF AXIS
xml_source <- "Buearu of Labor Statistics"
xml_date <- paste0("As of ", date_max_pretty)
xml_type <- "line" #line, bar, pie, etc
xml_qualifier <- " " #one line note, if needed



# Create chart node
unemployment_chart <- xml_new_root("chart")

#add children (title, subtitle, type)
xml_add_child(unemployment_chart, "title", xml_title)
xml_add_child(unemployment_chart, "subtitle", xml_subtitle)
xml_add_child(unemployment_chart, "type", xml_type)
xml_add_child(unemployment_chart, "x-axis", xml_xaxis)
xml_add_child(unemployment_chart, "y-axis", xml_yaxis)
xml_add_child(unemployment_chart, "y-max", xml_ymax)

# Add data rows
for (i in 1:nrow(civilian_unemployment_rate_forXML)) {
  row_node <- xml_add_child(unemployment_chart, "dataPoint")
  for (col_name in names(civilian_unemployment_rate_forXML)) {
    xml_add_child(row_node, col_name, as.character(civilian_unemployment_rate_forXML[i, col_name]))
  }
}


xml_add_child(unemployment_chart, "source", xml_source)
xml_add_child(unemployment_chart, "date", xml_date)
xml_add_child(unemployment_chart, "qualifier", xml_qualifier)


# Write XML to file
write_xml(unemployment_chart, "data/unemployment_rate.xml")




# CEU0000000001 Total nonfarm employment

total_nonfarm_employment <- get_series_table('CES0000000001', start_year = 2019, end_year = 2025) %>%
  mutate(month = str_sub(period, start=-2)) %>%
  mutate(year = as.character(year)) %>%
  mutate(month = as.character(month)) %>%
  mutate(date = paste0(year, "-", month, "-01")) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  select(date, value) %>%
  rename(label = date) %>%
  mutate(employment_mom = (value - lag(value))) %>%
  select(label, employment_mom) %>%
  rename(value = employment_mom) %>%
  mutate(value = value*1000) %>% 
  slice_tail(n = 25)


write.csv(total_nonfarm_employment, "data/monthly_change_nonfarm_employment.csv", row.names = FALSE)

#get min and max labels
date_max <- max(total_nonfarm_employment$label)
date_min <- min(total_nonfarm_employment$label)
value_max <- max(total_nonfarm_employment$value)
value_min <- min(total_nonfarm_employment$value)

#get pretty min date
date_min_pretty <- format(min(as.Date(total_nonfarm_employment$label)), "%b %Y")
date_min_pretty <-str_replace_all(date_min_pretty, " 0", " ")
#get pretty max date
date_max_pretty <- format(max(as.Date(total_nonfarm_employment$label)), "%b %Y")
date_max_pretty <-str_replace_all(date_max_pretty, " 0", " ")

#simplify to "label" and "value" column + "showLabel" + showValue" + "valueToShow" column...change columns 
total_nonfarm_employment_forXML <- total_nonfarm_employment %>% 
  mutate(showLabel = case_when(value == value_max ~ "1",
                               value == value_min ~ "1",
                               label == date_max ~ "1",
                               TRUE ~ "0")) %>% 
  mutate(showValue = case_when(value == value_max ~ "1",
                               value == value_min ~ "1",
                               label == date_max ~ "1",
                               TRUE ~ "0")) %>% 
  mutate(label = format(as.Date(label), "%b %Y")) %>% 
  mutate(label = str_replace_all(label, " 0", " ")) %>% 
  mutate(valueToShow = paste0(value/1000, "K"))


#get labels for x axis
labels = paste0(date_min_pretty, "|", date_max_pretty)

#convert US data to XML

#variables 
xml_title <- "Monthly changes in employment"
xml_subtitle <- "Nonfarm payroll employment, seasonally adjusted"
xml_xaxis <- labels #labels/values for x axis
xml_yaxis <- "100K|200K|300K" #labels/values for y axis, only fill out in necessary
xml_ymax <-  300000 #float value for max value OF AXIS
xml_source <- "Buearu of Labor Statistics"
xml_date <- paste0("As of ", date_max_pretty)
xml_type <- "line" #line, bar, pie, etc
xml_qualifier <- " " #one line note, if needed



# Create chart node
employment_chart <- xml_new_root("chart")

#add children (title, subtitle, type)
xml_add_child(employment_chart, "title", xml_title)
xml_add_child(employment_chart, "subtitle", xml_subtitle)
xml_add_child(employment_chart, "type", xml_type)
xml_add_child(employment_chart, "x-axis", xml_xaxis)
xml_add_child(employment_chart, "y-axis", xml_yaxis)
xml_add_child(employment_chart, "y-max", xml_ymax)

# Add data rows
for (i in 1:nrow(total_nonfarm_employment_forXML)) {
  row_node <- xml_add_child(employment_chart, "dataPoint")
  for (col_name in names(total_nonfarm_employment_forXML)) {
    xml_add_child(row_node, col_name, as.character(total_nonfarm_employment_forXML[i, col_name]))
  }
}


xml_add_child(employment_chart, "source", xml_source)
xml_add_child(employment_chart, "date", xml_date)
xml_add_child(employment_chart, "qualifier", xml_qualifier)


# Write XML to file
write_xml(employment_chart, "data/monthly_employment_change.xml")



