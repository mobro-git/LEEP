# save for later
wd = getwd()
# move here
setwd("C:/Users/sweisberg/Documents/LEEP/docs")


library(dplyr)

# list of NREL scenarios
scenarios = list.dirs("../data-extra/NREL")
# remove top directory
scenarios = scenarios[scenarios != "../data-extra/NREL"]

mapping = read.csv("../data-extra/NREL/mapping.csv")

dta_final = data.frame()
for (scen in scenarios) {
  files = list.files(scen)
  for (file in files) {
    if (grepl("national", file, fixed = TRUE)) {
      print(file)
      dta = read.csv(paste0(scen,"/",file))
      dta_processed = process_nrel_data(dta, substr(scen,20,nchar(scen)), mapping)
      dta_final = rbind(dta_final, dta_processed)
      print(nrow(dta_final))
    }
  }
}

# Manually generate some variables
dta_gas_cap = dta_final %>%
  filter(variable %in% c('Capacity|Electricity|Gas|CC|w/ CCS', 'Capacity|Electricity|Gas|CC|w/o CCS', 'Capacity|Electricity|Gas|CT|w/o CCS')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Capacity|Electricity|Gas')

dta_gas_cap_ccs = dta_final %>%
  filter(variable %in% c('Capacity|Electricity|Gas|CC|w/ CCS')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Capacity|Electricity|Gas|w/ CCS')

dta_gas_cap_noccs = dta_final %>%
  filter(variable %in% c('Capacity|Electricity|Gas|CC|w/o CCS', 'Capacity|Electricity|Gas|CT|w/o CCS')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Capacity|Electricity|Gas|w/o CCS')

dta_coal_cap = dta_final %>%
  filter(variable %in% c('Capacity|Electricity|Coal|w/ CCS', 'Capacity|Electricity|Coal|w/o CCS')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Capacity|Electricity|Coal')

dta_solar_cap = dta_final %>%
  filter(variable %in% c('Capacity|Electricity|Solar|CSP', 'Capacity|Electricity|Solar|PV|Rooftop', 'Capacity|Electricity|Solar|PV|Utility-Scale')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Capacity|Electricity|Solar')

dta_solar_cap_pv = dta_final %>%
  filter(variable %in% c('Capacity|Electricity|Solar|PV|Rooftop', 'Capacity|Electricity|Solar|PV|Utility-Scale')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Capacity|Electricity|Solar|PV')

dta_storage_cap = dta_final %>%
  filter(variable %in% c('Capacity|Electricity|Storage Capacity|Battery', 'Capacity|Electricity|Storage Capacity|PSH')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Capacity|Electricity|Storage Capacity')

dta_wind_cap = dta_final %>%
  filter(variable %in% c('Capacity|Electricity|Wind|Onshore', 'Capacity|Electricity|Wind|Offshore')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Capacity|Electricity|Wind')

# Same but for secondary energy
dta_gas_se = dta_final %>%
  filter(variable %in% c('Secondary Energy|Electricity|Gas|CC|w/ CCS', 'Secondary Energy|Electricity|Gas|CC|w/o CCS', 'Secondary Energy|Electricity|Gas|CT|w/o CCS')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Secondary Energy|Electricity|Gas')

dta_gas_se_ccs = dta_final %>%
  filter(variable %in% c('Secondary Energy|Electricity|Gas|CC|w/ CCS')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Secondary Energy|Electricity|Gas|w/ CCS')

dta_gas_se_noccs = dta_final %>%
  filter(variable %in% c('Secondary Energy|Electricity|Gas|CC|w/o CCS', 'Secondary Energy|Electricity|Gas|CT|w/o CCS')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Secondary Energy|Electricity|Gas|w/o CCS')

dta_coal_se = dta_final %>%
  filter(variable %in% c('Secondary Energy|Electricity|Coal|w/ CCS', 'Secondary Energy|Electricity|Coal|w/o CCS')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Secondary Energy|Electricity|Coal')

dta_solar_se = dta_final %>%
  filter(variable %in% c('Secondary Energy|Electricity|Solar|CSP', 'Secondary Energy|Electricity|Solar|PV|Rooftop', 'Secondary Energy|Electricity|Solar|PV|Utility-Scale')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Secondary Energy|Electricity|Solar')

dta_solar_se_pv = dta_final %>%
  filter(variable %in% c('Secondary Energy|Electricity|Solar|PV|Rooftop', 'Secondary Energy|Electricity|Solar|PV|Utility-Scale')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Secondary Energy|Electricity|Solar|PV')

dta_storage_se = dta_final %>%
  filter(variable %in% c('Secondary Energy|Electricity|Storage Capacity|Battery', 'Secondary Energy|Electricity|Storage Capacity|PSH')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Secondary Energy|Electricity|Storage Capacity')

dta_wind_se = dta_final %>%
  filter(variable %in% c('Secondary Energy|Electricity|Wind|Onshore', 'Secondary Energy|Electricity|Wind|Offshore')) %>%
  group_by(region, year, unit, scenario, model) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = 'Secondary Energy|Electricity|Wind')

dta_final = rbind(dta_final,
                  dta_wind_se, dta_storage_se, dta_solar_se, dta_solar_se_pv, dta_coal_se, dta_gas_se_noccs, dta_gas_se_ccs, dta_gas_se,
                  dta_wind_cap, dta_storage_cap, dta_solar_cap, dta_solar_cap_pv, dta_coal_cap, dta_gas_cap_noccs, dta_gas_cap_ccs, dta_gas_cap)

dta_final %>% write.csv("../data-extra/NREL/ReEDS_compiled.csv", row.names = FALSE)


process_nrel_data = function(dta, scenario_name, mapping) {
  # I don't want these columns
  dta = dta %>% select(-c(o.g.s_MW,o.g.s_MWh,re_ct_MW,re_ct_MWh, canada_MWh)) %>%
    gather("nrel","value",battery_MW:co2_c_mmt) %>%
    left_join(mapping, by = "nrel") %>%
    select(-nrel) %>%
    mutate(value = case_when(
      unit == "GW" ~ value / 1000, # convert from MW to GW
      unit == "EJ/yr" ~ value * 3.6e-9, # convert from MWh to EJ
      TRUE ~ value * 1 # the rest will be emissions, which are in the right units
    )) %>%
    mutate(scenario = scenario_name, model = "ReEDS")

  colnames(dta)[1] = "region"
  colnames(dta)[2] = "year"

  return(dta)

}

# dta2 = process_nrel_data(dta, 'test', mapping)




# back to where we started
setwd(wd)
