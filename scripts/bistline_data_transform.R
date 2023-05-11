#####
##
## this code transforms the Bistline database into tall format,
## renames variables to match the EMF37 template, and converts units as necessary
##
#####

library(tidyverse)
library(readxl)
library(xlsx)

#####
##
## Importing Bistline data and converts units
##
#####

data.emissions <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "Emissions") %>%
  gather("year", "value", 4:11) %>%
  mutate(table = "emissions")

data.co2captured <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "CO2 Captured") %>%
  gather("year", "value", 4:9) %>%
  mutate(table = "co2_captured")

# data.costs <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "Costs") %>%
#   gather("year", "value", 4:9) %>%
#   mutate(table = "costs")

data.elecdemand <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "Electricity Demand") %>%
  gather("year", "value", 4:9) %>%
  mutate(table = "elec_demand") %>%
  mutate(cal_value = value*0.0036) %>%
  # converting from TWh to EJ/Yr
  select(-value) %>%
  rename(value = cal_value) %>%
  mutate(
    unit = case_when(
      unit == "TWh" ~ "EJ/yr",
      TRUE ~ "unit"))

data.trnelcdemand <- data.elecdemand %>%
  filter(variable %in% c("Electricity Demand - Light-Duty Vehicles", "Electricity Demand - Other Transport")) %>%
  mutate(variable = "Electricity Demand - Transportation") %>%
  group_by(scenario,model,variable,unit,year,table) %>%
  summarise(value = sum(value))

data.nox <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "NOx") %>%
  gather("year", "value", 4:9) %>%
  mutate(table = "nox")

# data.so2 <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "SO2") %>%
#   gather("year", "value", 4:9) %>%
#   mutate(table = "so2")

data.ffconsump <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "Fossil Fuel Consumption") %>%
  gather("year", "value", 4:10) %>%
  mutate(table = "ff_consump") %>%
  mutate(cal_value = value*1.05505585262) %>%
  # converting from quads to EJ/Yr
  select(-value) %>%
  rename(value = cal_value) %>%
  mutate(
    unit = case_when(
      unit == "quads" ~ "EJ/yr",
      TRUE ~ "unit"))

data.transport <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "Transport") %>%
  mutate(unit = '%') %>%
  gather("year", "value", 4:7)

combined.data <- full_join(data.emissions, data.co2captured) %>%
  full_join(data.elecdemand) %>%
  full_join(data.ffconsump) %>%
  full_join(data.nox) %>%
  full_join(data.transport) %>%
  full_join(data.trnelcdemand) %>%
  rename("Bistline Variable" = variable)


#####
##
## Renaming variables to match EMF Formatting structure
##
#####


var.mapping <- read_excel("data-extra/VariableMapping.xlsx")

combined.data.2 <- left_join(combined.data, var.mapping, by = "Bistline Variable") %>%
  mutate(
    unit = case_when(
      unit == "Mt-CO2e/yr" ~ "Mt CO2/yr",
      unit == "Mt-CO2/yr" ~ "Mt CO2/yr",
      unit == "Mt-NOx/yr" ~ "Mt NOx/yr",
      TRUE ~ unit
    )
  ) %>%
  rename(variable = "Template Variable") %>%
  select(-table, -Notes, -"Bistline Variable") %>%
  mutate(region = "United States") %>%
  filter(!is.na(variable)) %>%
  filter(!year < 2025)

write.csv(combined.data.2, "data-raw/model-runs/bistline_ira_tall.csv", row.names = FALSE)







