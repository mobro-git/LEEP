#####
##
## this code transforms the Bistline database into tall format,
## renames variables to match the EMF37 template, and converts units as necessary
##
#####

library(tidyverse)
library(readxl)
library(xlsx)
devtools::load_all()

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

fuel_emissions_oldname <- full_join(data.emissions, data.co2captured) %>%
  full_join(data.elecdemand) %>%
  full_join(data.ffconsump) %>%
  full_join(data.nox) %>%
  full_join(data.transport) %>%
  full_join(data.trnelcdemand) %>%
  group_by(scenario,model,variable,unit,year,table) %>%
  summarise(value = sum(value)) %>%
  rename("Bistline Variable" = variable)

#####
##
## Renaming variables to match EMF Formatting structure
##
#####

var.mapping <- read_excel("data-extra/ira_comparison_raw/VariableMapping.xlsx")

fuel_emissions <- left_join(fuel_emissions_oldname, var.mapping, by = "Bistline Variable") %>%
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
  filter(!year < 2025) %>%
  group_by(scenario,model,variable,unit,year,region) %>%
  summarise(value = sum(value))

#####
##
## generation, capacity, and capacity change sheets
##
#####

### modeled data

generation <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "generation") %>%
  mutate(`Secondary Energy|Electricity|Nuclear` = `Secondary Energy|Electricity|Nuclear1`+`Secondary Energy|Electricity|Nuclear2`) %>%
  select(-`Secondary Energy|Electricity|Nuclear1`, -`Secondary Energy|Electricity|Nuclear2`) %>%
  mutate(`Secondary Energy|Electricity|Gas|w/o CCS` = `Secondary Energy|Electricity|Gas|w/o CCS1`+`Secondary Energy|Electricity|Gas|w/o CCS2`) %>%
  select(-`Secondary Energy|Electricity|Gas|w/o CCS1`, -`Secondary Energy|Electricity|Gas|w/o CCS2`) %>%
  mutate(`Secondary Energy|Electricity|Solar` = `Secondary Energy|Electricity|Solar1`+`Secondary Energy|Electricity|Solar2`) %>%
  select(-`Secondary Energy|Electricity|Solar1`, -`Secondary Energy|Electricity|Solar2`) %>%
  pivot_longer(cols = 5:16, names_to = "variable", values_to = "value") %>%
  mutate(value = value*0.0036,
         unit = "EJ/yr")  %>% # conversion from TWh to EJ/yr
  mutate(region = "United States") %>%
  select(scenario,model,variable,unit,year,region,value)

capacity <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "capacity") %>%
  mutate(`Capacity|Electricity|Nuclear` = `Capacity|Electricity|Nuclear`+`Capacity|Electricity|Nuclear2`) %>%
  select(-`Capacity|Electricity|Nuclear2`) %>%
  mutate(`Capacity|Electricity|Gas|w/o CCS` = `Capacity|Electricity|Gas|w/o CCS`+`Capacity|Electricity|Gas|w/o CCS2`) %>%
  select(-`Capacity|Electricity|Gas|w/o CCS2`) %>%
  mutate(`Capacity|Electricity|Solar` = `Capacity|Electricity|Solar`+`Capacity|Electricity|Solar2`) %>%
  select(-`Capacity|Electricity|Solar2`) %>%
  pivot_longer(cols = 5:17, names_to = "variable", values_to = "value") %>%
  mutate(region = "United States") %>%
  select(scenario,model,variable,unit,year,region,value)

capacity_change <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "capacity change") %>%
  pivot_longer(cols = 4:12, names_to = "model", values_to = "value") %>%
  mutate(year = 2035) %>%
  mutate(region = "United States") %>%
  select(scenario,model,variable,unit,year,region,value)

modeled_data = rbind(generation,capacity,capacity_change) %>%
  mutate(year = as.character(year))

### historic data

hist_generation <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "historic generation") %>%
  mutate(`Secondary Energy|Electricity|Other` = `Secondary Energy|Electricity|Other`+`Secondary Energy|Electricity|Other2`) %>%
  select(-`Secondary Energy|Electricity|Other2`) %>%
  pivot_longer(cols = 5:15, names_to = "variable", values_to = "value") %>%
  mutate(value = value/1000,
         unit = "TWh") %>% # conversion milltion kWh to TWh
  mutate(value = value*0.0036,
         unit = "EJ/yr")  %>% # conversion from TWh to EJ/yr
  mutate(model = "EIA",
         region = "United States") %>%
  select(scenario,model,variable,unit,year,region,value)

hist_capacity_change <- read_xlsx("data-extra/ira_comparison_raw/ira_comparison.xlsx", sheet = "historic capacity change") %>%
  pivot_longer(cols = 5:77, names_to = "year", values_to = "value") %>%
  mutate(region = "United States") %>%
  select(scenario,model,variable,unit,year,region,value)

historic_data = rbind(hist_generation,hist_capacity_change)


#####
##
## add capacity/generation to other workbooks
##
#####

all_reported = rbind(
  fuel_emissions,
  historic_data,
  modeled_data
) %>%
  ungroup() %>%
  arrange(variable)


#####
##
## apportion power sector emissions to sectors based on electricity use
##
#####

indirect = all_reported %>%
  filter(
    variable %in% c(
      "Emissions|CO2|Energy|Supply|Electricity",
      "Final Energy|Electricity",
      "Final Energy|Buildings|Electricity",
      "Final Energy|Industry|Electricity",
      "Final Energy|Transportation|Electricity"
    )
  ) %>%
  select(-unit) %>%
  pivot_wider(names_from = "variable", values_from = "value")

calc_indirect = indirect %>%
  # end-use percent consumption of total electricity
  mutate(
    bld_per_elc = `Final Energy|Buildings|Electricity`/`Final Energy|Electricity`,
    ind_per_elc = `Final Energy|Industry|Electricity`/`Final Energy|Electricity`,
    trn_per_elc = `Final Energy|Transportation|Electricity`/`Final Energy|Electricity`) %>%
  # end-use apportionment of electricity emissions based on consmption of electricity
  mutate(
    `Emissions|CO2|Energy|Demand|Buildings|Indirect` = `bld_per_elc` * `Emissions|CO2|Energy|Supply|Electricity`,
    `Emissions|CO2|Energy|Demand|Industry|Indirect` = `ind_per_elc` * `Emissions|CO2|Energy|Supply|Electricity`,
    `Emissions|CO2|Energy|Demand|Transportation|Indirect` = `trn_per_elc` * `Emissions|CO2|Energy|Supply|Electricity`
    ) %>%
  mutate(unit = "Mt CO2/yr") %>%
  select(
    scenario,model,unit,year,region,
    `Emissions|CO2|Energy|Demand|Buildings|Indirect`,
    `Emissions|CO2|Energy|Demand|Industry|Indirect`,
    `Emissions|CO2|Energy|Demand|Transportation|Indirect`
  )

indirect_long = calc_indirect %>%
  pivot_longer(cols = 6:8, names_to = "variable", values_to = "value") %>%
  select(scenario,model,variable,unit,year,region,value)

#####
##
## all variables
##
#####

all_variables = rbind(all_reported, indirect_long)

write.csv(all_variables, "data-raw/model-runs/bistline_ira_tall.csv", row.names = FALSE)







