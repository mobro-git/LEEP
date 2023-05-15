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
    mutate(scenario = scenario_name)

  colnames(dta)[1] = "region"
  colnames(dta)[2] = "year"

  return(dta)

}

dta2 = process_nrel_data(dta, 'test', mapping)




# back to where we started
setwd(wd)
