#### transforming bistline database into tall format

library(tidyverse)
library(readxl)
library(xlsx)

data.emissions <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "Emissions") %>%
  gather("year", "value", 4:9) %>%
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

# data.nox <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "NOx") %>%
#   gather("year", "value", 4:9) %>%
#   mutate(table = "nox")
#
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

combined.data <- full_join(data.emissions, data.co2captured) %>%
  # full_join(data.costs) %>%
  full_join(data.elecdemand) %>%
  # full_join(data.nox) %>%
  # full_join(data.so2) %>%
  full_join(data.ffconsump)
  #write.csv("bistline_ira.csv")

write.xlsx(combined.data, "bistline_ira_tall.xlsx", sheetName = "data", col.names = TRUE)

# transformed.data <- combined.data %>%
#   case_match(
#     "variable",
#     c("CO2 Captured - Biofuels with CCS") ~ "Carbon Sequestration|CCS|Biomass|Energy|Supply|Biomass Liquids",
#     c("CO2 Captured - Hydrogen with CCS") ~ "Carbon Sequestration|CCS|Biomass|Energy|Supply|Hydrogen",
#     c("CO2 Captured - Industrial CCS") ~ "Carbon Sequestration|CCS|Biomass|Energy|Demand|Industry",
#     c("CO2 Captured - Other Negative Emissions") ~ "Carbon Sequestration|Other",
#     c("CO2 Captured - Power: Biomass") ~ "Carbon Sequestration|CCS|Biomass|Energy|Supply|Biomass Solids",
#     c("CO2 Emissions - Buildings") ~ "Emissions|CO2|Energy|Demand|Buildings",
#     c("CO2 Emissions - Electric") ~ "Emissions|CO2|Energy|Supply|Electricity",
#     c("CO2 Emissions - Industry") ~ "Emissions|CO2|Energy|Demand|Industry",
#     c("CO2 Emissions - Non-Energy CO2") ~ "Emissions|CO2|Industrial Processes",
#     c("CO2 Emissions - Other") ~ "Emissions|CO2|Other",
#     c("CO2 Emissions - Transport") ~ "Emissions|CO2|Energy|Demand|Transportation",
#     c("Electricity Demand - Buildings") ~ "Final Energy|Buildings",
#     c("Electricity Demand - Industry") ~ "Final Energy|Industry",
#     c("Fossil Consumption - Coal") ~ "Final Energy|Coal",
#     c("Fossil Consumption - Natural Gas") ~ "Final Energy|Gas",
#     c("Fossil Consumption - Petroleum") ~ "Final Energy|Oil",
#     c("CO2 Captured - Biofuels with CCS") ~ "Carbon Sequestration|CCS|Biomass|Energy|Supply|Biomass Liquids",
#     c("CO2 Captured - Biofuels with CCS") ~ "Carbon Sequestration|CCS|Biomass|Energy|Supply|Biomass Liquids",
#     c("CO2 Captured - Biofuels with CCS") ~ "Carbon Sequestration|CCS|Biomass|Energy|Supply|Biomass Liquids",
#     .default = as.character(x))


transformed.data <- combined.data %>%
  mutate(
    variable = case_when(
    variable == "CO2 Captured - Biofuels with CCS" ~ "Carbon Sequestration|CCS|Biomass|Energy|Supply|Biomass Liquids",
    variable == "CO2 Captured - Hydrogen with CCS" ~ "Carbon Sequestration|CCS|Biomass|Energy|Supply|Hydrogen",
    variable == "CO2 Captured - Industrial CCS" ~ "Carbon Sequestration|CCS|Biomass|Energy|Demand|Industry",
    variable == "CO2 Captured - Other Negative Emissions" ~ "Carbon Sequestration|Other",
    variable == "CO2 Captured - Power: Biomass" ~ "Carbon Sequestration|CCS|Biomass|Energy|Supply|Biomass Solids",
    variable == "CO2 Emissions - Buildings" ~ "Emissions|CO2|Energy|Demand|Buildings",
    variable == "CO2 Emissions - Electric" ~ "Emissions|CO2|Energy|Supply|Electricity",
    variable == "CO2 Emissions - Industry" ~ "Emissions|CO2|Energy|Demand|Industry",
    variable == "CO2 Emissions - Non-Energy CO2" ~ "Emissions|CO2|Industrial Processes",
    variable == "CO2 Emissions - Other" ~ "Emissions|CO2|Other",
    variable == "CO2 Emissions - Transport" ~ "Emissions|CO2|Energy|Demand|Transportation",
    variable == "Electricity Demand - Buildings" ~ "Final Energy|Buildings",
    variable == "Electricity Demand - Industry" ~ "Final Energy|Industry",
    variable == "Fossil Consumption - Coal" ~ "Final Energy|Coal",
    variable == "Fossil Consumption - Natural Gas" ~ "Final Energy|Gas",
    variable == "Fossil Consumption - Petroleum" ~ "Final Energy|Oil",
    variable == "CO2 Captured - Biofuels with CCS" ~ "Carbon Sequestration|CCS|Biomass|Energy|Supply|Biomass Liquids",
    variable == "CO2 Captured - Biofuels with CCS" ~ "Carbon Sequestration|CCS|Biomass|Energy|Supply|Biomass Liquids",
    variable == "CO2 Captured - Biofuels with CCS" ~ "Carbon Sequestration|CCS|Biomass|Energy|Supply|Biomass Liquids",
    TRUE ~ "delete")) %>%
  mutate(
    unit = case_when(
      unit == "Mt-CO2e/yr" ~ "Mt CO2/yr",
      unit == "Mt-CO2/yr" ~ "Mt CO2/yr",
      TRUE ~ unit
    )
  ) %>%
  filter(variable != "delete") %>%
  select(-table)

write.csv(transformed.data, "data-raw/model-runs/bistline_ira_tall.csv", row.names = FALSE)


##### variable renaming to EMF37 standard ######
#
#
# "CO2 Captured - Biofuels with CCS" to "Carbon Sequestration|CCS|Biomass|Energy|Supply|Biomass Liquids"
# "CO2 Captured - Hydrogen with CCS"  to "Carbon Sequestration|CCS|Biomass|Energy|Supply|Hydrogen"
# "CO2 Captured - Industrial CCS" to "Carbon Sequestration|CCS|Biomass|Energy|Demand|Industry"
# "CO2 Captured - Other CCS" to ""
# "CO2 Captured - Other Negative Emissions" to "Carbon Sequestration|Other"
# "CO2 Captured - Power: Biomass" to "Carbon Sequestration|CCS|Biomass|Energy|Supply|Biomass Solids"
# "CO2 Captured - Power: Coal" to "
# "CO2 Captured - Power: Gas" to "
# "CO2 Emissions - Buildings" to "Emissions|CO2|Energy|Demand|Buildings"
# "CO2 Emissions - Electric" to "Emissions|CO2|Energy|Supply|Electricity"
# "CO2 Emissions - Industry" to "Emissions|CO2|Energy|Demand|Industry"
# "CO2 Emissions - Non-Energy CO2" to "Emissions|CO2|Industrial Processes"
# "CO2 Emissions - Other" to "Emissions|CO2|Other"
# "CO2 Emissions - Sink" to "
# "CO2 Emissions - Transport" to "Emissions|CO2|Energy|Demand|Transportation"
# "Costs - 45Q" to "
# "Costs - Buildings Non-Energy" to "
# "Costs - Electric" to "
# "Costs - Industry Non-Energy" to "
# "Costs - ITC" to "
# "Costs - Natural Gas" to "
# "Costs - Other Incentives" to "
# "Costs - Petroleum" to "
# "Costs - PTC" to "
# "Costs - Transport Credits" to "
# "Costs - Transport Non-Energy" to "
# "Economy NOx" to "
# "Economy SO2" to "
# "Electricity Demand - Buildings" to "Final Energy|Buildings"
# "Electricity Demand - Industry" to "Final Energy|Industry"
# "Electricity Demand - Light-Duty Vehicles" to "
# "Electricity Demand - Other" to "
# "Electricity Demand - Other Transport" to "
# "Electricity Demand - TOTAL" to "
# "Fossil Consumption - Coal" to "Final Energy|Coal"
# "Fossil Consumption - Natural Gas" to "Final Energy|Gas"
# "Fossil Consumption - Petroleum" to "Final Energy|Oil"
# "Non-CO2 GHG" to "
# "Power NOx" to "
# "Power SO2" to "





