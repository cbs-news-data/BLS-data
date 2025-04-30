#install pkgs if needed
# install.packages("blsR")
# install.packages("tidyverse")
# install.packages("stringr")
# install.packages("xml2")

library(blsR)
library(tidyverse)
library(stringr)
library(xml2)

#CUUR0000SA0 = not seasonally adjusted (using this for YoY change)
#CUSR0000SA0 = seasonally adjusted

# Get CPI (all urban consumers, U.S. city average, all items, not seasonally adjusted)
CPI <- get_series_table('CUUR0000SA0', start_year = 2019, end_year = 2025) %>% 
  mutate(month = str_sub(period, start=-2)) %>% 
  mutate(year = as.character(year)) %>% 
  mutate(month = as.character(month)) %>% 
  mutate(date = paste0(year, "-", month, "-01")) %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(date) %>%
  mutate(inflation_yoy = (value / lag(value, 12) - 1) * 100) %>% 
  mutate(inflation_yoy = round(inflation_yoy, digits = 1)) %>% 
  filter(!is.na(inflation_yoy)) %>% 
  select(date, inflation_yoy) %>% 
  rename(label=date,
         value=inflation_yoy)


write.csv(CPI, "data/inflation_yoy.csv", row.names = FALSE)


#get min and max labels
date_max <- max(CPI$label)
date_min <- min(CPI$label)
value_max <- max(CPI$value)
value_min <- min(CPI$value)

#get pretty min date
date_min_pretty <- format(min(as.Date(CPI$label)), "%b %Y")
date_min_pretty <-str_replace_all(date_min_pretty, " 0", " ")
#get pretty max date
date_max_pretty <- format(max(as.Date(CPI$label)), "%b %Y")
date_max_pretty <-str_replace_all(date_max_pretty, " 0", " ")

#simplify to "label" and "value" column + "showLabel" + showValue" + "valueToShow" column...change columns 
inflation_yoy_forXML <- CPI %>% 
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
xml_title <- "Inflation year-over-year"
xml_subtitle <- "U.S. city average for all items"
xml_xaxis <- labels #labels/values for x axis
xml_yaxis <- "3%|6%|9%" #labels/values for y axis, only fill out in necessary
xml_ymax <-  9 #float value for max value OF AXIS
xml_source <- "Buearu of Labor Statistics"
xml_date <- paste0("As of ", date_max_pretty)
xml_type <- "bar" #line, bar, pie, etc
xml_qualifier <- "Not seasonally adjusted" #one line note, if needed



# Create chart node
inflation_chart <- xml_new_root("chart")

#add children (title, subtitle, type)
xml_add_child(inflation_chart, "title", xml_title)
xml_add_child(inflation_chart, "subtitle", xml_subtitle)
xml_add_child(inflation_chart, "type", xml_type)
xml_add_child(inflation_chart, "x-axis", xml_xaxis)
xml_add_child(inflation_chart, "y-axis", xml_yaxis)
xml_add_child(inflation_chart, "y-max", xml_ymax)

# Add data rows
for (i in 1:nrow(inflation_yoy_forXML)) {
  row_node <- xml_add_child(inflation_chart, "dataPoint")
  for (col_name in names(inflation_yoy_forXML)) {
    xml_add_child(row_node, col_name, as.character(inflation_yoy_forXML[i, col_name]))
  }
}


xml_add_child(inflation_chart, "source", xml_source)
xml_add_child(inflation_chart, "date", xml_date)
xml_add_child(inflation_chart, "qualifier", xml_qualifier)


# Write XML to file
write_xml(inflation_chart, "data/inflation_yoy.xml")
