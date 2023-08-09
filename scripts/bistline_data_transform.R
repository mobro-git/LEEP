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

bistline_wrkbk = "data-extra/ira_comparison_raw/ira_comparison_updated_08082023.xlsx"
historic_wrkbk = "data-extra/ira_comparison_raw/bistline-historic.xlsx"

#####
##
## generation, capacity, capacity change
##
#####

generation = read_xlsx(bistline_wrkbk, sheet = "generation") %>%
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
  relocate_standard_col_order()

capacity = read_xlsx(bistline_wrkbk, sheet = "capacity") %>%
  mutate(`Capacity|Electricity|Nuclear` = `Capacity|Electricity|Nuclear`+`Capacity|Electricity|Nuclear2`) %>%
  select(-`Capacity|Electricity|Nuclear2`) %>%
  mutate(`Capacity|Electricity|Gas|w/o CCS` = `Capacity|Electricity|Gas|w/o CCS`+`Capacity|Electricity|Gas|w/o CCS2`) %>%
  select(-`Capacity|Electricity|Gas|w/o CCS2`) %>%
  mutate(`Capacity|Electricity|Solar` = `Capacity|Electricity|Solar`+`Capacity|Electricity|Solar2`) %>%
  select(-`Capacity|Electricity|Solar2`) %>%
  pivot_longer(cols = 5:17, names_to = "variable", values_to = "value") %>%
  mutate(region = "United States") %>%
  relocate_standard_col_order()

capacity_change = read_xlsx(bistline_wrkbk, sheet = "capacity change") %>%
  pivot_longer(cols = 4:13, names_to = "model", values_to = "value") %>%
  mutate(year = 2035) %>%
  mutate(region = "United States") %>%
  relocate_standard_col_order()

ipm_epa_gen_cap = read_xlsx(bistline_wrkbk, sheet = "gen cap IPM-EPA") %>%
  pivot_longer(cols = 7:11, names_to = "year", values_to = "value") %>%
  mutate(
    value = case_when(
      unit == "TWh" ~ value*0.0036, # conversion from TWh to EJ/yr
      TRUE~value),
    unit = case_when(
      unit == "TWh" ~ "EJ/yr",
      TRUE~unit)) %>%
  select(-`variable-bistline`) %>%
  group_by(model,scenario,region,variable,unit,year) %>%
  summarise(value = sum(value)) %>%
  relocate_standard_col_order()

gen_cap = rbind(generation,capacity,capacity_change,ipm_epa_gen_cap)

#####
##
## emissions and captured co2
##
#####

emissions <- read_xlsx(bistline_wrkbk, sheet = "emissions") %>%
  pivot_longer(cols = 7:12, names_to = "year", values_to = "value") %>%
  relocate_standard_col_order() %>%
  select(-`variable-bistline`)

ccs <- read_xlsx(bistline_wrkbk, sheet = "ccs") %>%
  pivot_longer(cols = 7:12, names_to = "year", values_to = "value") %>%
  relocate_standard_col_order() %>%
  select(-`variable-bistline`)

co2 = rbind(emissions, ccs)

nox_so2 = read_xlsx(bistline_wrkbk, sheet = "non-co2") %>%
  pivot_longer(cols = 6:11, names_to = "year", values_to = "value") %>%
  relocate_standard_col_order()

hi_low <- read_xlsx(bistline_wrkbk, sheet = "emissions sensitivities") %>%
  pivot_longer(cols = 7:12, names_to = "year", values_to = "value") %>%
  relocate_standard_col_order() %>%
  select(-`variable-bistline`)

emissions = rbind(co2, nox_so2, hi_low)

#####
##
## electricity demand
##
#####

elcdmd <- read_xlsx(bistline_wrkbk, sheet = "elc demand") %>%
  pivot_longer(cols = 7:12, names_to = "year", values_to = "value") %>%
  mutate(
    value = case_when(
      unit == "TWh" ~ value*0.0036, # conversion from TWh to EJ/yr
      TRUE~value),
    unit = case_when(
      unit == "TWh" ~ "EJ/yr",
      TRUE~unit)) %>%
  relocate_standard_col_order() %>%
  select(-`variable-bistline`)

trn_elc = elcdmd %>%
  filter(variable %in% c("Final Energy|Transportation|Electricity|Other Transport",
                         "Final Energy|Transportation|Passenger|Road|Electricity")) %>%
  mutate(variable = "Final Energy|Transportation|Electricity") %>%
  group_by(scenario,model,year,unit,region,variable) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  relocate_standard_col_order()

elc_nosens = rbind(elcdmd, trn_elc)

elcdmd_sens <- read_xlsx(bistline_wrkbk, sheet = "elc demand sensitivities") %>%
  pivot_longer(cols = 7:12, names_to = "year", values_to = "value") %>%
  mutate(
    value = case_when(
      unit == "TWh" ~ value*0.0036, # conversion from TWh to EJ/yr
      TRUE~value),
    unit = case_when(
      unit == "TWh" ~ "EJ/yr",
      TRUE~unit)) %>%
  relocate_standard_col_order() %>%
  select(-`variable-bistline`)

trn_elc_sens = elcdmd_sens %>%
  filter(variable %in% c("Final Energy|Transportation|Electricity|Other Transport",
                         "Final Energy|Transportation|Passenger|Road|Electricity")) %>%
  mutate(variable = "Final Energy|Transportation|Electricity") %>%
  group_by(scenario,model,year,unit,region,variable) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  relocate_standard_col_order()

elc_sens = rbind(elcdmd_sens,trn_elc_sens)

elc = rbind(elc_nosens, elc_sens)

#####
##
## ev share
##
#####

ev_share <- read_xlsx(bistline_wrkbk, sheet = "ev share") %>%
  pivot_longer(cols = 6:8, names_to = "year", values_to = "value") %>%
  relocate_standard_col_order()

#####
##
## primary energy
##
#####

fossil <- read_xlsx(bistline_wrkbk, sheet = "fossil") %>%
  pivot_longer(cols = 7:12, names_to = "year", values_to = "value") %>%
  mutate(value = value*1.05505585262,
         unit = "EJ/yr") %>% # converting from quads to EJ/Yr
  relocate_standard_col_order() %>%
  select(-`variable-bistline`)

############################
##
## all modeled data
##
#############################

all_modeled = rbind(gen_cap, emissions, elc, ev_share, fossil) %>%
  filter(!is.na(value))

#############################
##
## modeled data calculations
##
#############################


#####
##
## apportion power sector emissions to sectors based on electricity use
##
#####

indirect = all_modeled %>%
  filter(
    variable %in% c(
      "Emissions|CO2|Energy|Supply|Electricity",
      "Final Energy|Electricity",
      "Final Energy|Buildings|Electricity",
      "Final Energy|Industry and Fuel Production|Electricity",
      "Final Energy|Transportation|Electricity"
    )
  ) %>%
  select(-unit) %>%
  pivot_wider(names_from = "variable", values_from = "value")

calc_indirect = indirect %>%
  # end-use percent consumption of total electricity
  mutate(
    bld_per_elc = `Final Energy|Buildings|Electricity`/`Final Energy|Electricity`,
    ind_per_elc = `Final Energy|Industry and Fuel Production|Electricity`/`Final Energy|Electricity`,
    trn_per_elc = `Final Energy|Transportation|Electricity`/`Final Energy|Electricity`) %>%
  # end-use apportionment of electricity emissions based on consmption of electricity
  mutate(
    `Emissions|CO2|Energy|Demand|Buildings|Indirect` = `bld_per_elc` * `Emissions|CO2|Energy|Supply|Electricity`,
    `Emissions|CO2|Energy|Demand|Industry and Fuel Production|Indirect` = `ind_per_elc` * `Emissions|CO2|Energy|Supply|Electricity`,
    `Emissions|CO2|Energy|Demand|Transportation|Indirect` = `trn_per_elc` * `Emissions|CO2|Energy|Supply|Electricity`
  ) %>%
  mutate(unit = "Mt CO2/yr") %>%
  select(
    scenario,model,unit,year,region,
    `Emissions|CO2|Energy|Demand|Buildings|Indirect`,
    `Emissions|CO2|Energy|Demand|Industry and Fuel Production|Indirect`,
    `Emissions|CO2|Energy|Demand|Transportation|Indirect`
  )

indirect_long = calc_indirect %>%
  pivot_longer(cols = 6:8, names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  relocate_standard_col_order()

#####
##
## emissions sums
##
#####

nrg_co2_var = c("Emissions|CO2|Energy|Demand|Buildings",
                "Emissions|CO2|Energy|Demand|Industry and Fuel Production",
                "Emissions|CO2|Energy|Demand|Transportation",
                "Emissions|CO2|Energy|Supply|Electricity")

co2_models = c("EPS-EI", "GCAM-CGS", "MARKAL-NETL", "NEMS-RHG", "RIO-REPEAT")

nrg_co2 = all_modeled %>%
  filter(variable %in% nrg_co2_var) %>%
  filter(model %in% co2_models | model == "REGEN-EPRI") %>%
  mutate(variable = "Emissions|CO2|Energy") %>%
  group_by(scenario,model,variable,unit,year,region) %>%
  summarise(value = sum(value)) %>%
  ungroup()

all_co2 = all_modeled %>%
  filter(variable %in% nrg_co2_var | variable == "Emissions|CO2|Industrial Processes" | variable == "Emissions|CO2|Other") %>%
  filter(model %in% co2_models) %>%
  mutate(variable = "Emissions|CO2") %>%
  group_by(scenario,model,variable,unit,year,region) %>%
  summarise(value = sum(value)) %>%
  ungroup()

ghg = rbind(emissions, all_co2) %>%
  select(-unit) %>%
  filter(variable %in% c("Emissions|CO2", "Emissions|Non-CO2 GHG")) %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  mutate(`Emissions|GHG` = `Emissions|CO2` + `Emissions|Non-CO2 GHG`) %>%
  select(-`Emissions|CO2`,-`Emissions|Non-CO2 GHG`) %>%
  pivot_longer(col = `Emissions|GHG`, names_to = "variable", values_to = "value") %>%
  mutate(unit = "Mt CO2e/yr") %>%
  relocate_standard_col_order()

net_ghg_model = c("GCAM-CGS", "MARKAL-NETL", "NEMS-RHG", "RIO-REPEAT", "EPS-EI")

sink = all_modeled %>%
  filter(variable == "Carbon Sequestration|LULUCF" &
           model %in% net_ghg_model)

ghg_net = ghg %>%
  filter(model %in% net_ghg_model) %>%
  mutate(variable = "Emissions|GHG|Net") %>%
  group_by(model,scenario,region,unit,year,variable) %>%
  summarise(value=sum(value)) %>%
  ungroup() %>%
  relocate_standard_col_order()

summed_emissions = rbind(nrg_co2, all_co2, ghg, ghg_net)


#############################
##
## all modeled and calculated data
##
#############################

all_data = rbind(all_modeled, indirect_long, summed_emissions) %>%
  filter(!is.na(value))

write.csv(all_data, "data-raw/model-runs/bistline_ira_tall.csv", row.names = FALSE)


########################################################### Historic

#############################
##
## historic data
##
#############################

hist_ev_share = read_xlsx(historic_wrkbk, sheet = "ev share") %>%
  pivot_longer(cols = 6:18, names_to = "year", values_to = "value") %>%
  relocate_standard_col_order()

hist_generation <- read_xlsx(historic_wrkbk, sheet = "generation") %>%
  mutate(`Secondary Energy|Electricity|Other` = `Secondary Energy|Electricity|Other`+`Secondary Energy|Electricity|Other2`) %>%
  select(-`Secondary Energy|Electricity|Other2`) %>%
  pivot_longer(cols = 5:15, names_to = "variable", values_to = "value") %>%
  mutate(value = value/1000,
         unit = "TWh") %>% # conversion milltion kWh to TWh
  mutate(value = value*0.0036,
         unit = "EJ/yr")  %>% # conversion from TWh to EJ/yr
  mutate(model = "EIA",
         region = "United States") %>%
  relocate_standard_col_order()

hist_capacity_change <- read_xlsx(historic_wrkbk, sheet = "capacity change") %>%
  pivot_longer(cols = 5:76, names_to = "year", values_to = "value") %>%
  mutate(region = "United States") %>%
  mutate(model = "EIA") %>%
  relocate_standard_col_order()

hist_fossil = read_xlsx(historic_wrkbk, sheet = "primary energy") %>%
  mutate(model = "EIA",
         scenario = "Historic") %>%
  pivot_longer(cols = c("coal","gas","oil"), names_to = "var", values_to = "quads") %>%
  mutate(variable = case_when(
    var == "coal" ~ "Primary Energy|Coal",
    var == "gas" ~ "Primary Energy|Gas",
    var == "oil" ~ "Primary Energy|Oil"
  )) %>%
  # converting from quads to EJ/Yr
  mutate(value = quads*1.05505585262,
         unit = "EJ/yr",
         region = "United States") %>%
  relocate_standard_col_order() %>%
  select(-var,-quads)

historic = rbind(hist_ev_share, hist_generation, hist_capacity_change, hist_fossil)

#write.csv(historic, "data-raw/model-runs/EIA-IEA-bistline_historic.csv", row.names = FALSE)

